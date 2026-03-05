package smithy4s_codegen.compilation

import cats.effect.IO
import cats.effect.kernel.Resource
import coursier._
import cats.syntax.all._
import coursier.parse.DependencyParser
import fs2.io.file.{Files, Path}
import smithy4s_codegen.BuildInfo
import smithy4s_codegen.generation.CodegenResult

import java.nio.charset.StandardCharsets

class ScalaCliCompiler private (scalaCliClasspath: String) {

  def compile(
      files: List[(os.RelPath, CodegenResult)],
      smithy4sVersion: String
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
        }
        .productR(runScalaCli(dir, smithy4sVersion))
    }

  private val tempDir: Resource[IO, Path] =
    Files[IO].tempDirectory(None, "smithy4s-compile", None)

  private def runScalaCli(
      dir: Path,
      smithy4sVersion: String
  ): IO[Either[List[String], String]] =
    IO.blocking {
      val javaExecutable =
        ProcessHandle.current().info().command().orElse("java")
      val process = new ProcessBuilder(
        java.util.Arrays.asList(
          javaExecutable,
          "-cp",
          scalaCliClasspath,
          "scala.cli.ScalaCli",
          "compile",
          "--scala",
          "2.13",
          "--dep",
          s"com.disneystreaming.smithy4s::smithy4s-core:$smithy4sVersion",
          dir.toString
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
    }

  private def stripAnsi(s: String): String =
    s.replaceAll("\u001B\\[[;\\d]*[mGKHF]", "")
}

object ScalaCliCompiler {
  def make: IO[ScalaCliCompiler] =
    IO.blocking {
      val dep = DependencyParser
        .dependency(
          s"org.virtuslab.scala-cli::cli:${BuildInfo.scalaCliVersion}",
          defaultScalaVersion = "3"
        )
        .left
        .map(err => sys.error(s"Failed to parse scala-cli dependency: $err"))
        .merge

      val classpath = Fetch()
        .addDependencies(dep)
        .run()

      classpath.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)
    }.map(new ScalaCliCompiler(_))
}
