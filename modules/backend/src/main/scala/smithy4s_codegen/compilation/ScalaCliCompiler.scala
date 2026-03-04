package smithy4s_codegen.compilation

import cats.effect.IO
import cats.effect.kernel.Resource
import smithy4s_codegen.generation.{CodegenResult, Smithy4s}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class ScalaCliCompiler private (generator: Smithy4s) {

  def compile(
      dependencies: List[String],
      content: String,
      smithy4sVersion: String
  ): IO[Either[List[String], String]] =
    IO.fromEither(
      generator.generate(dependencies, content).left.map(_.map(_.getMessage))
    ).flatMap(compileFiles(_, smithy4sVersion))

  private def compileFiles(
      files: List[(os.RelPath, CodegenResult)],
      smithy4sVersion: String
  ): IO[Either[List[String], String]] =
    withTempDir { dir =>
      IO.blocking {
        files.foreach { case (relPath, result) =>
          val targetDir = dir.resolve(relPath.toString)
          Files.createDirectories(targetDir)
          Files.writeString(
            targetDir.resolve(s"${result.name}.scala"),
            result.content
          )
        }
      } >> runScalaCli(dir, smithy4sVersion)
    }

  private def runScalaCli(
      dir: Path,
      smithy4sVersion: String
  ): IO[Either[List[String], String]] =
    IO.blocking {
      val javaExecutable = ProcessHandle.current().info().command().orElse("java")
      val process = new ProcessBuilder(
        java.util.Arrays.asList(
          javaExecutable,
          "-cp",
          System.getProperty("java.class.path"),
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

  private def withTempDir[A](f: Path => IO[A]): IO[A] =
    Resource
      .make(IO.blocking(Files.createTempDirectory("smithy4s-compile")))(dir =>
        IO.blocking(deleteDirectory(dir))
      )
      .use(f)

  private def deleteDirectory(path: Path): Unit =
    if (Files.exists(path))
      Files
        .walk(path)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
}

object ScalaCliCompiler {
  def make(generator: Smithy4s): IO[ScalaCliCompiler] =
    IO.pure(new ScalaCliCompiler(generator))
}
