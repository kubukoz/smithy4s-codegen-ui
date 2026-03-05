package smithy4s_codegen.compilation

import cats.effect.IO
import smithy4s_codegen.BuildInfo
import smithy4s_codegen.generation.CodegenResult
import weaver._

object ScalaCliCompilerTest extends SimpleIOSuite {

  test("compile a trivial Scala file") {
    ScalaCliCompiler.make(BuildInfo.scalaCliVersion).flatMap { compiler =>
      val files = List(
        os.rel / "hello" -> CodegenResult(
          namespace = "hello",
          name = "Hello",
          content = "object Hello { val x: Int = 42 }"
        )
      )
      compiler.compile(files).map {
        case Right(output) =>
          expect(clue(output).contains(s"Compiling project (Scala ${BuildInfo.scalaVersion}")) and
            expect(output.contains(s"Compiled project (Scala ${BuildInfo.scalaVersion}"))
        case Left(errors) => failure(errors.mkString("\n"))
      }
    }
  }
}
