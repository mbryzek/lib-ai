package cilists

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** The quarantined and slow specs a build runs with, and what a proof run's report says.
  *
  * Platform holds the lists; whatever starts a build fetches them into a file and exports its path
  * as `CI_TEST_LISTS`. The file's first line is the lists version and every line after it is
  * `quarantined <spec>`, `slow <spec>` or `proof <spec>`, where a spec is a suite by the name its
  * JUnit report gives it, which is the class name sbt selects a suite by.
  *
  * No sbt type appears here on purpose. This file is compiled twice: by the meta-build, where
  * [[CiTestListsPlugin]] turns it into a `Tests.Filter`, and by `testkit` as a test source, where
  * `CiTestListsSpec` asserts every rule below. It therefore stays inside the syntax Scala 2.12 and
  * Scala 3 share.
  */
object CiTestLists {

  /** How many times a proof runs a spec alone, and how many times with the rest of its subproject. */
  val ProofRuns: Int = 20

  final case class Lists(version: String, quarantined: Set[String], slow: Set[String], proof: Set[String]) {
    def listed: Set[String] = quarantined ++ slow ++ proof
  }

  private val Version = """(?:version )?(\d+)""".r
  private val Entry = """(quarantined|slow|proof) (\S+)""".r

  /** A file that is not exactly the format is refused whole. A line this does not understand is a
    * spec the build would otherwise run while the lists say it must not.
    */
  def parse(text: String): Either[String, Lists] = {
    val lines = text.split("\n", -1).toList.map(_.stripSuffix("\r"))
    lines match {
      case Version(version) :: entries =>
        entries.zipWithIndex.foldLeft[Either[String, Lists]](Right(Lists(version, Set.empty, Set.empty, Set.empty))) {
          case (Right(lists), (line, index)) =>
            line match {
              case Entry("quarantined", spec) => Right(lists.copy(quarantined = lists.quarantined + spec))
              case Entry("slow", spec) => Right(lists.copy(slow = lists.slow + spec))
              case Entry("proof", spec) => Right(lists.copy(proof = lists.proof + spec))
              case blank if blank.trim.isEmpty => Right(lists)
              case other => Left(s"line ${index + 2} is neither quarantined, slow nor proof: $other")
            }
          case (failed, _) => failed
        }
      case first :: _ => Left(s"the first line is not a lists version: $first")
      case Nil => Left("the file is empty")
    }
  }

  def read(file: File): Either[String, Lists] =
    if (file.isFile) parse(new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8))
    else Left("no such file")

  /** Whether a build runs `spec`.
    *
    * With no lists nothing is left out, which is how the session fixing a quarantined spec runs it.
    * With lists, a quarantined spec and a slow spec are left out; `slowOnly` is `CI_SLOW_SPECS=1`
    * and inverts that to the slow specs alone. A quarantined spec runs nowhere, so it stays out of
    * the slow run too. A `proof` spec is one the build is about to prove, so its own build runs it.
    */
  def runs(lists: Option[Lists], slowOnly: Boolean, spec: String): Boolean =
    lists match {
      case None => !slowOnly
      case Some(l) if slowOnly => l.slow(spec) && !l.quarantined(spec)
      case Some(l) => l.proof(spec) || !(l.quarantined(spec) || l.slow(spec))
    }

  /** The listed specs no suite in the build matches, in a stable order. */
  def unknown(lists: Lists, defined: Set[String]): Seq[String] =
    (lists.listed -- defined).toSeq.sorted

  /** Each proof spec with the subproject that defines it, in a stable order. A proof spec no
    * subproject defines is not here: the pull request deleted it, and there is nothing to prove.
    */
  def proofPlan(lists: Lists, definedByProject: Map[String, Set[String]]): Seq[(String, String)] =
    for {
      spec <- lists.proof.toSeq.sorted
      project <- definedByProject.toSeq.sortBy(_._1).collectFirst { case (id, specs) if specs(spec) => id }.toSeq
    } yield (project, spec)

  /** The report ScalaTest's JUnit reporter leaves for a suite. */
  def reportName(spec: String): String = s"TEST-$spec.xml"

  /** The report one proof run leaves, beside the suite's own. `PROOF-` is what tells the ledger
    * writer the file is one run of a proof and not the build's row for the spec; `mode` is `alone`
    * or `load` and `run` counts from 1.
    */
  def proofReportName(mode: String, run: Int, spec: String): String =
    f"PROOF-$mode-$run%02d-$spec.xml"

  private val Testsuite = """<testsuite\b[^>]*>""".r

  private def count(tag: String, attribute: String): Option[Int] =
    (attribute + """="(\d+)"""").r.findFirstMatchIn(tag).map(_.group(1).toInt)

  /** Whether a JUnit report is a run in which the suite ran at least one test and none failed. A
    * suite that ran nothing proved nothing.
    */
  def reportPassed(xml: String): Boolean =
    Testsuite.findFirstIn(xml).exists { tag =>
      count(tag, "errors").contains(0) && count(tag, "failures").contains(0) && count(tag, "tests").exists(_ > 0)
    }

  /** The report written for a run that left none of its own, so that every run leaves one. */
  def missingReport(spec: String): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<testsuite errors="1" failures="0" name="$spec" tests="1" time="0">
       |  <testcase classname="$spec" name="ci-proof" time="0">
       |    <error message="the run left no report for this suite" type="ci-proof"/>
       |  </testcase>
       |</testsuite>
       |""".stripMargin
}
