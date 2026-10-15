package cilists

import sbt.*
import sbt.Keys.*
import sbt.plugins.JvmPlugin

import scala.util.control.NonFatal

/** `ciTest`: every subproject's tests, then one rerun of every spec that failed (ISS-19887,
  * DES-18577 "Rerun").
  *
  * A spec that fails and then passes its rerun is a FLAKE: one `ci-flake: <suite>` line, and it does
  * not fail the build. A spec that fails its rerun too prints `ci-rerun: failed again: <suite>` and
  * does. `ci-rerun: done` is printed once a rerun has run, whatever it found, and is what the fleet's
  * whole-build flake retry reads to stand down.
  *
  * A ROUND IS RERUN ONLY WHEN THE FAILING SPECS ARE THE WHOLE FAILURE. The suite runs as each
  * subproject's `Test / executeTests`, which answers per suite rather than throwing, so a failed
  * spec is named by sbt itself and never read out of a log. Anything that is not a spec — a compile
  * failure, a task that failed, a subproject whose result is red while no suite in it is — leaves
  * nothing a rerun can clear, and the build stays red without one.
  *
  * Each subproject's failed specs rerun as one `testOnly`, which runs the same `Tests.Setup` and the
  * same `Tests.Filter` as `test`. A green `testOnly` makes every spec in it a flake; a red one is
  * split by the reports the rerun wrote, and a spec whose report the rerun did not write failed
  * again, since nothing says it passed.
  *
  * BEFORE A RERUN, each spec's report is kept as `FIRST-<suite>.xml` beside it: the rerun rewrites
  * `TEST-<suite>.xml`, and the fleet's test ledger reads a flaked spec's first-run seconds, failed
  * tests and failure signature from the kept copy. A copy that fails costs the ledger that detail and
  * nothing else.
  *
  * `ci-test-phase: <n>s` is the wall time of the suite alone, before any rerun: a repository's slow
  * ceiling is a share of it, and a rerun is not the phase every pull request waits on.
  *
  * This file is a copy of devops `templates/sbt-test-health/project/CiTestRerunPlugin.scala`, held
  * to it by the repository's `ci/build.sh`; edit it there.
  */
object CiTestRerunPlugin extends AutoPlugin {

  override def trigger = allRequirements

  override def requires = JvmPlugin

  object autoImport {
    val ciProjectTests: TaskKey[(String, Tests.Output)] =
      taskKey[(String, Tests.Output)]("This subproject's id and what its tests answered, suite by suite")
    val ciAllTests: TaskKey[Seq[(String, Tests.Output)]] =
      taskKey[Seq[(String, Tests.Output)]]("Every subproject's tests, by subproject id")
  }

  import autoImport.*

  override def projectSettings: Seq[Setting[?]] = Seq(
    ciProjectTests := {
      val id = thisProject.value.id
      val output = (Test / executeTests).value
      val resultLogger = (Test / testResultLogger).value
      val log = streams.value.log
      // The summary `test` prints, without the exception `test` throws: the verdict is `ciTest`'s.
      try resultLogger.run(log, output, s"$id / Test / test")
      catch { case NonFatal(_) => () }
      id -> output
    }
  )

  private val everyProject = ScopeFilter(inAnyProject)

  override def globalSettings: Seq[Setting[?]] = Seq(
    ciAllTests := ciProjectTests.all(everyProject).value,
    commands += ciTest,
  )

  /** The specs one subproject's run failed. `None` when the subproject is red and no suite in it
    * is, which is a failure no rerun can reach.
    */
  def failedSpecs(output: Tests.Output): Option[Seq[String]] = {
    val failed = output.events.toSeq.collect { case (spec, r) if r.result != TestResult.Passed => spec }.sorted
    if (output.overall != TestResult.Passed && failed.isEmpty) None else Some(failed)
  }

  private def attempt[T](run: => (State, T)): Option[(State, T)] =
    try Some(run)
    catch { case NonFatal(_) => None }

  private def ciTest: Command = Command.command("ciTest") { state =>
    val started = System.nanoTime()
    attempt(Project.extract(state).runTask(Global / ciAllTests, state)) match {
      case None =>
        println("ci-rerun: nothing to rerun — the suite did not run to a result, which is not a spec's failure")
        state.fail
      case Some((next, outputs)) =>
        println(s"ci-test-phase: ${(System.nanoTime() - started) / 1000000000L}s")
        val byProject = outputs.sortBy(_._1).map { case (id, output) => id -> failedSpecs(output) }
        byProject.collectFirst { case (id, None) => id } match {
          case Some(id) =>
            println(s"ci-rerun: nothing to rerun — $id failed and no suite in it did")
            next.fail
          case None =>
            val failing = byProject.collect { case (id, Some(specs)) if specs.nonEmpty => id -> specs }
            if (failing.isEmpty) next else rerunAll(next, failing)
        }
    }
  }

  private def rerunAll(state: State, failing: Seq[(String, Seq[String])]): State = {
    val (last, outcomes) = failing.foldLeft((state, Seq.empty[(String, Boolean)])) {
      case ((current, acc), (project, specs)) =>
        val (next, passed) = rerun(current, project, specs)
        (next, acc ++ passed)
    }
    outcomes.foreach {
      case (spec, true) => println(s"ci-flake: $spec")
      case (spec, false) => println(s"ci-rerun: failed again: $spec")
    }
    println("ci-rerun: done")
    val flakes = outcomes.count(_._2)
    if (outcomes.forall(_._2)) {
      println(s"sbt: green after rerunning every spec that failed ($flakes flake${if (flakes == 1) "" else "s"})")
      last
    } else last.fail
  }

  /** One subproject's failed specs, rerun once in one `testOnly`, each answered passed or not. */
  private def rerun(state: State, project: String, specs: Seq[String]): (State, Seq[(String, Boolean)]) = {
    val extracted = Project.extract(state)
    val ref = extracted.structure.allProjectRefs.find(_.project == project).getOrElse(sys.error(s"no project $project"))
    val reports = extracted.get(ref / target) / "test-reports"
    specs.foreach { spec =>
      val report = reports / CiTestLists.reportName(spec)
      try { if (report.isFile) IO.copyFile(report, reports / s"FIRST-$spec.xml") }
      catch {
        case NonFatal(e) => println(s"ci-rerun: the first run of $spec could not be kept: ${e.getMessage}")
      }
    }
    println(s"ci-rerun: $project/testOnly ${specs.mkString(" ")}")
    val since = System.currentTimeMillis()
    attempt(extracted.runInputTask(ref / Test / testOnly, specs.mkString(" ", " ", ""), state)) match {
      case Some((next, _)) => (next, specs.map(_ -> true))
      case None =>
        val answered = specs.map { spec =>
          val report = reports / CiTestLists.reportName(spec)
          val passed = report.isFile && report.lastModified() >= since - 1000 &&
            CiTestLists.reportPassed(IO.read(report))
          spec -> passed
        }
        (state, answered)
    }
  }
}
