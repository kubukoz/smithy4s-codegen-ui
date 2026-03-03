package smithy4s_codegen.compilation

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Mutex
import smithy4s_codegen.generation.{CodegenResult, Smithy4s}

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path}
import java.security.Permission

private class ExitException(val code: Int)
    extends SecurityException(s"exit $code")

class ScalaCliCompiler private (generator: Smithy4s, mutex: Mutex[IO]) {

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
    mutex.lock.surround {
      IO.blocking {
        val output = new ByteArrayOutputStream()
        val ps = new PrintStream(output, false, java.nio.charset.StandardCharsets.UTF_8)
        val oldOut = System.out
        val oldErr = System.err
        val oldSm = System.getSecurityManager()
        System.setOut(ps)
        System.setErr(ps)
        installNoExitSecurityManager()
        val exitCode =
          try {
            scala.cli.ScalaCli.main(
              Array(
                "compile",
                "--scala",
                "2.13",
                "--dep",
                s"com.disneystreaming.smithy4s::smithy4s-core:$smithy4sVersion",
                dir.toString
              )
            )
            0
          } catch {
            case e: ExitException => e.code
          } finally {
            System.setOut(oldOut)
            System.setErr(oldErr)
            System.setSecurityManager(oldSm)
            ps.close()
          }

        val outputStr = stripAnsi(output.toString(java.nio.charset.StandardCharsets.UTF_8))
        if (exitCode == 0) Right(outputStr)
        else Left(List(outputStr))
      }
    }

  @scala.annotation.nowarn("cat=deprecation")
  private def installNoExitSecurityManager(): Unit =
    System.setSecurityManager(new SecurityManager {
      override def checkExit(status: Int): Unit = throw new ExitException(status)
      override def checkPermission(perm: Permission): Unit = ()
      override def checkPermission(perm: Permission, ctx: AnyRef): Unit = ()
    })

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
    Mutex[IO].map(new ScalaCliCompiler(generator, _))
}
