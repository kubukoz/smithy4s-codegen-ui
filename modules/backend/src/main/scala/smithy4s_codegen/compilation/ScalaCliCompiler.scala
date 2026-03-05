package smithy4s_codegen.compilation

import cats.effect.IO
import cats.effect.kernel.Resource
import scala.concurrent.duration._
import scala.concurrent.Future
import coursier._
import cats.syntax.all._
import coursier.parse.DependencyParser
import fs2.io.file.{Files, Path}
import fs2.io.process.{ProcessBuilder => Fs2ProcessBuilder, Processes}
import smithy4s_codegen.generation.CodegenResult
import cats.effect.std.Supervisor
import os.RelPath

trait ScalaCliCompiler {
  def compile(
      files: List[(os.RelPath, CodegenResult)],
      extraDeps: List[String] = Nil
  ): IO[Either[List[String], String]]
}

private class ScalaCliCompilerImpl(scalaCliClasspath: String)
    extends ScalaCliCompiler {

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
        } *> runScalaCli(dir, extraDeps, files.size)
    }

  private val tempDir: Resource[IO, Path] =
    Files[IO].tempDirectory(None, "smithy4s-compile", None)

  private def runScalaCli(
      dir: Path,
      extraDeps: List[String],
      fileCount: Int
  ): IO[Either[List[String], String]] = {
    val javaExecutable = ProcessHandle.current().info().command().orElse("java")
    val depArgs = extraDeps.flatMap(dep => List("--dep", dep))
    val args =
      List(
        "-cp",
        scalaCliClasspath,
        "scala.cli.ScalaCli",
        "compile",
        "--scala",
        smithy4s_codegen.BuildInfo.scalaVersion
      ) ++
        depArgs ++
        List(dir.toString)

    IO.println(
      s"ScalaCliCompiler: starting compilation of $fileCount file(s)"
    ) >>
      Processes[IO]
        .spawn(Fs2ProcessBuilder(javaExecutable, args))
        .use { process =>
          (
            process.stdout
              .observe(fs2.io.stdout)
              .through(fs2.text.utf8.decode)
              .compile
              .string,
            process.stderr
              .observe(fs2.io.stderr)
              .through(fs2.text.utf8.decode)
              .compile
              .string,
            process.exitValue
          ).mapN { (stdout, stderr, exitCode) =>
            val output = stripAnsi(stdout + stderr)
            if (exitCode == 0) Right(output)
            else Left(List(output))
          }
        }
        .timeout(20.seconds)
        .flatTap {
          case Right(_) => IO.println("ScalaCliCompiler: compilation succeeded")
          case Left(errors) =>
            IO.println(
              s"ScalaCliCompiler: compilation failed:\n${errors.mkString("\n")}"
            )
        }
  }

  private def stripAnsi(s: String): String =
    s.replaceAll("\u001B\\[[;\\d]*[mGKHF]", "")
}

object ScalaCliCompiler {
  def make(scalaCliVersion: String): IO[ScalaCliCompiler] =
    IO.executionContext
      .flatMap { implicit ec =>
        val dep = DependencyParser
          .dependency(
            s"org.virtuslab.scala-cli::cli:$scalaCliVersion",
            defaultScalaVersion = "3"
          )
          .left
          .map(err => sys.error(s"Failed to parse scala-cli dependency: $err"))
          .merge

        IO.println(s"ScalaCliCompiler: fetching scala-cli $scalaCliVersion") >>
          IO.fromFuture(IO {
            Fetch()
              .addDependencies(dep)
              .future()
          }).map {
            _.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)
          }.flatTap { _ =>
            IO.println("ScalaCliCompiler: scala-cli classpath ready")
          }.map(new ScalaCliCompilerImpl(_))
      }

  extension (a: IO[ScalaCliCompiler]) {
    def supervised(using sup: Supervisor[IO]): IO[ScalaCliCompiler] =
      a.supervise(sup)
        .flatTap { f =>
          IO.println(s"Building compiler in the background... ${f}")
        }
        .map { fib =>
          new {
            def compile(
                files: List[(RelPath, CodegenResult)],
                extraDeps: List[String]
            ): IO[Either[List[String], String]] =
              fib.join
                .flatMap(_.embedError)
                .flatMap(_.compile(files, extraDeps))
          }
        }
  }
}
