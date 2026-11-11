package com.bryzek.ai.claude

import java.nio.file.{Files, Path, Paths}
import scala.quoted.*
import scala.util.Try

/** Where a [[Question]] is declared in its feature's code: the repository (`mbryzek/acumen`), the file's path from that
  * repository's root and the 1-based line of the `Question.choice`, `Question.score` or `Question.yesNo` call. A console
  * links it to the line, so a person reading a question's thresholds or fallback reaches the code that sets them.
  *
  * Captured at compile time by the given [[DeclarationSite.here]], which every declaring method takes as a `using`
  * parameter: the feature writes nothing. `repository` is absent when the compiled tree is not a git checkout whose
  * `origin` is a GitHub repository; `path` is then relative to the directory the compiler ran in.
  */
final case class DeclarationSite(repository: Option[String], path: String, line: Int) {
  require(path.trim.nonEmpty, "DeclarationSite path must not be empty")
  require(line >= 1, s"DeclarationSite line[$line] must be 1 or more")

  override def toString: String = s"${repository.fold("")(_ + ":")}$path:$line"
}

object DeclarationSite {

  /** The call site this given is resolved at. */
  inline given here: DeclarationSite = ${ hereImpl }

  private def hereImpl(using q: Quotes): Expr[DeclarationSite] = {
    val (sourcePath, sourceName, startLine) = expansion
    val file = Try(Paths.get(sourcePath).toAbsolutePath.normalize).toOption
    val (repository, path) = file.fold((Option.empty[String], sourceName))(locate)
    val line: Int = startLine + 1
    val repositoryExpr: Expr[Option[String]] = Expr(repository)
    val pathExpr: Expr[String] = Expr(path)
    val lineExpr: Expr[Int] = Expr(line)
    '{ DeclarationSite($repositoryExpr, $pathExpr, $lineExpr) }
  }

  /** The source file's path and name and the 0-based line the macro expands at: the call taking the given. */
  private def expansion(using q: Quotes): (String, String, Int) = {
    import q.reflect.*
    val position = Position.ofMacroExpansion
    (position.sourceFile.path, position.sourceFile.name, position.startLine)
  }

  /** The repository and repository-relative path of `file`: the nearest enclosing git checkout, else the compiler's
    * working directory with no repository.
    */
  private[claude] def locate(file: Path): (Option[String], String) =
    gitRoot(file.getParent) match {
      case Some(root) => (originRepository(root), slashed(root.relativize(file)))
      case None =>
        val cwd = Paths.get("").toAbsolutePath.normalize
        (None, slashed(if (file.startsWith(cwd)) cwd.relativize(file) else file.getFileName))
    }

  private def slashed(path: Path): String = path.toString.replace('\\', '/')

  private def gitRoot(dir: Path): Option[Path] =
    Iterator.iterate(dir)(_.getParent).takeWhile(_ != null).find(d => Files.exists(d.resolve(".git")))

  /** `owner/name` of the checkout's `origin` remote when it is a GitHub repository. A worktree's `.git` is a file naming
    * its git directory, whose `commondir` holds the shared config.
    */
  private def originRepository(root: Path): Option[String] =
    Try {
      val config = gitDir(root.resolve(".git")).resolve("config")
      Option.when(Files.isRegularFile(config))(Files.readString(config)).flatMap(originUrl).flatMap(githubRepository)
    }.toOption.flatten

  private def gitDir(dotGit: Path): Path =
    if (Files.isDirectory(dotGit)) dotGit
    else {
      val pointer = Files.readString(dotGit).trim.stripPrefix("gitdir:").trim
      val dir = dotGit.getParent.resolve(pointer).normalize
      val common = dir.resolve("commondir")
      if (Files.isRegularFile(common)) dir.resolve(Files.readString(common).trim).normalize else dir
    }

  private[claude] def originUrl(config: String): Option[String] = {
    val lines = config.linesIterator.map(_.trim).toSeq
    val section = lines.indexWhere(_ == "[remote \"origin\"]")
    Option
      .when(section >= 0)(lines.drop(section + 1).takeWhile(!_.startsWith("[")))
      .flatMap(_.collectFirst {
        case l if l.startsWith("url") && l.contains("=") => l.substring(l.indexOf('=') + 1).trim
      })
  }

  private val GithubUrl =
    """^(?:https?://(?:[^@/]+@)?github\.com/|git@github\.com:|ssh://git@github\.com/)([^/]+)/([^/]+?)(?:\.git)?/?$""".r

  private[claude] def githubRepository(url: String): Option[String] = url match {
    case GithubUrl(owner, name) => Some(s"$owner/$name")
    case _ => None
  }
}
