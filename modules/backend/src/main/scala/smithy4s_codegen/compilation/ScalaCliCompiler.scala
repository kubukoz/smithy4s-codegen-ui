package smithy4s_codegen.compilation

import cats.effect.IO
import cats.effect.kernel.Resource
import scala.concurrent.Future
import coursier._
import cats.syntax.all._
import coursier.parse.DependencyParser
import fs2.io.file.{Files, Path}
import smithy4s_codegen.generation.CodegenResult

import java.nio.charset.StandardCharsets

class ScalaCliCompiler private (scalaCliClasspath: String) {

  def compile(
      files: List[(os.RelPath, CodegenResult)],
      extraDeps: List[String] = Nil
  ): IO[Either[List[String], String]] =
    tempDir.use { dir =>
      files
        .traverse_ { case (relPath, result) =>
          val targetDir = dir / relPath.toString
          Files[IO].createDirectories(targetDir) *>
            fs2.Stream
              .emit(result.content)
              .through(Files[IO].writeUtf8(targetDir / s"${result.name}.scala"))
              .compile
              .drain
        } *> runScalaCli(dir, extraDeps)
    }

  private val tempDir: Resource[IO, Path] =
    Files[IO].tempDirectory(None, "smithy4s-compile", None)

  private def runScalaCli(
      dir: Path,
      extraDeps: List[String]
  ): IO[Either[List[String], String]] =
    IO.println("ScalaCliCompiler: starting compilation") >>
    IO.blocking {
      val javaExecutable =
        ProcessHandle.current().info().command().orElse("java")
      val depArgs = extraDeps.flatMap(dep => List("--dep", dep))
      val process = new ProcessBuilder(
        java.util.Arrays.asList(
          (List(javaExecutable, "-cp", scalaCliClasspath, "scala.cli.ScalaCli", "compile", "--scala", "2.13")
            ++ depArgs
            ++ List(dir.toString))*
        )
      ).redirectErrorStream(true)
        .start()

      // Drain the output stream in a separate thread to avoid deadlock when the
      // subprocess fills the pipe buffer before completing.
      var output = ""
      val reader = new Thread(() =>
        output = new String(
          process.getInputStream.readAllBytes(),
          StandardCharsets.UTF_8
        )
      )
      reader.start()
      val exitCode = process.waitFor()
      reader.join()

      val outputStr = stripAnsi(output)
      if (exitCode == 0) Right(outputStr)
      else Left(List(outputStr))
    }.flatTap {
      case Right(_)    => IO.println("ScalaCliCompiler: compilation succeeded")
      case Left(errors) => IO.println(s"ScalaCliCompiler: compilation failed:\n${errors.mkString("\n")}")
    }

  private def stripAnsi(s: String): String =
    s.replaceAll("\u001B\\[[;\\d]*[mGKHF]", "")
}

object ScalaCliCompiler {
  def make(scalaCliVersion: String): IO[ScalaCliCompiler] =
    IO.executionContext.flatMap { implicit ec =>
      val dep = DependencyParser
        .dependency(
          s"org.virtuslab.scala-cli::cli:$scalaCliVersion",
          defaultScalaVersion = "3"
        )
        .left
        .map(err => sys.error(s"Failed to parse scala-cli dependency: $err"))
        .merge

      IO.println(s"ScalaCliCompiler: fetching scala-cli $scalaCliVersion") >>
      IO.fromFuture(IO(Future {
        Fetch()
          .addDependencies(dep)
          .run()
          .map(_.getAbsolutePath)
          .mkString(java.io.File.pathSeparator)
      })).flatTap(_ => IO.println("ScalaCliCompiler: scala-cli classpath ready"))
        .map(new ScalaCliCompiler(_))
    }
}
