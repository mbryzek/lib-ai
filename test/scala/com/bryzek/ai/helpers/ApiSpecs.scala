package helpers

import java.io.File
import java.nio.file.Files
import scala.annotation.tailrec
import scala.sys.process.*

/** A contract spec, read from mbryzek/api, which holds every one at `spec/<category>/<name>.json`.
  *
  * A clone of `api` beside this checkout (the `~/code/ai/<feature>/` directory of a cross-repository
  * change) is read first, as `dev api` reads it; otherwise `api`'s main through `dev repo cat`, which
  * honours `DEV_REPO_PENDING`.
  */
object ApiSpecs {

  /** @param path a path inside `api`, e.g. `spec/ai/claude-response.json` */
  def read(path: String): String =
    sibling(path) match {
      case Some(file) => Files.readString(file.toPath)
      case None => Seq("dev", "repo", "cat", "api", path).!!
    }

  private def sibling(path: String): Option[File] = {
    val api = new File(repoRoot.getParentFile, "api")
    Option(new File(api, path)).filter(f => f.isFile && new File(api, ".git").exists())
  }

  /** The directory holding `build.sbt`, found by walking up from the working directory. */
  private lazy val repoRoot: File = {
    @tailrec def up(dir: File): File =
      if (new File(dir, "build.sbt").isFile) dir
      else
        Option(dir.getParentFile) match {
          case Some(parent) => up(parent)
          case None => sys.error("Could not locate the repository root (no build.sbt in any parent directory)")
        }

    up(new File(sys.props("user.dir")).getCanonicalFile)
  }
}
