package smithy4s_codegen.components.pages

import com.raquo.airstream.ownership.ManualOwner
import com.raquo.laminar.api.L._
import com.raquo.airstream.flatten.SwitchingStrategy
import smithy4s_codegen.api._
import smithy4s_codegen.components.CodeEditor
import smithy4s_codegen.components.CodeEditor.ValidationResult
import smithy4s_codegen.components.CodeViewer
import smithy4s_codegen.components.EditorContent

object Home {
  def apply(
      api: SmithyCodeGenerationService[EventStream],
      config: EventStream[Either[Throwable, GetConfigurationOutput]]
  ) = {
    val editor = new CodeEditor(config)
    val viewer = new CodeViewer()

    locally {
      implicit val owner = new ManualOwner
      editor.editorContent.signal.foreach(editor.writePermalink)
    }

    val validate: EventStream[CodeEditor.ValidationResult] =
      editor.editorContent.signal
        .composeChanges(_.debounce(2000))
        .flatMapSwitch { content =>
          api
            .smithyValidate(content.code, Some(content.deps))
            .map(_ => CodeEditor.ValidationResult.Success(content))
            .recover {
              case InvalidSmithyContent(errors) =>
                Some(CodeEditor.ValidationResult.Failed(errors))
              case ex =>
                Some(CodeEditor.ValidationResult.UnknownFailure(ex))
            }
        }

    val convertedToSmithy4s: EventStream[CodeEditor.Smithy4sConversionResult] =
      validate.compose {
        _.collect { case ValidationResult.Success(content) => content }
          .flatMapSwitch { content =>
            api
              .smithy4sConvert(content.code, Some(content.deps))
              .map(r =>
                CodeEditor.Smithy4sConversionResult.Success(r.generated)
              )
              .recover { ex =>
                Some(CodeEditor.Smithy4sConversionResult.UnknownFailure(ex))
              }
          }
      }

    val compileClicked = new EventBus[Unit]

    val scalaVersionVar: Var[String] = Var(smithy4s_codegen.BuildInfo.scalaVersion)

    val compileResult: EventStream[CodeEditor.CompileResult] =
      EventStream.merge(
        compileClicked.events.mapTo(CodeEditor.CompileResult.Loading),
        compileClicked.events
          .withCurrentValueOf(editor.editorContent.signal)
          .withCurrentValueOf(scalaVersionVar.signal)
          .flatMapSwitch { case ((content: EditorContent), scalaVersion: String) =>
            api
              .smithy4sCompile(content.code, Some(content.deps), Some(scalaVersion))
              .map(r => CodeEditor.CompileResult.Success(r.output))
              .recover {
                case CompileError(errors) =>
                  Some(CodeEditor.CompileResult.Failed(errors))
                case InvalidSmithyContent(errors) =>
                  Some(CodeEditor.CompileResult.Failed(errors))
                case ex =>
                  Some(CodeEditor.CompileResult.UnknownFailure(ex))
              }
          }
      )

    val compileResultVar: Var[CodeEditor.CompileResult] =
      Var(CodeEditor.CompileResult.NotStarted)

    val hasGeneratedCode: Var[Boolean] = Var(false)

    val (validateResultIcon, validateResultErrors) =
      editor.validationResult(validate)

    div(
      compileResult --> compileResultVar,
      convertedToSmithy4s.collect {
        case _: CodeEditor.Smithy4sConversionResult.Success => true
      } --> hasGeneratedCode,
      cls := "container mx-auto h-full py-2 flex",
      div(
        cls := "h-full p-2 relative basis-1/2 flex flex-col",
        editor.component,
        div(
          cls := "absolute top-2 right-3",
          validateResultIcon
        ),
        p(
          cls := "shrink-0",
          "Contribute here: ",
          a(
            href := "https://github.com/kubukoz/smithy4s-codegen-ui",
            target := "_blank",
            "https://github.com/kubukoz/smithy4s-codegen-ui"
          )
        )
      ),
      div(
        cls := "h-auto p-2 basis-1/2 overflow-x-scroll",
        validateResultErrors,
        child <-- hasGeneratedCode.signal.map {
          case false => emptyNode
          case true =>
            div(
              cls := "mb-4",
              div(
                cls := "flex items-center gap-2 mb-2",
                button(
                  cls := "px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 disabled:opacity-50",
                  onClick.mapTo(()) --> compileClicked,
                  disabled <-- compileResultVar.signal.map(
                    _ ==
                      CodeEditor.CompileResult.Loading
                  ),
                  "Compile"
                ),
                label(
                  cls := "text-sm text-gray-600",
                  "Scala version: "
                ),
                input(
                  cls := "px-2 py-2 border border-gray-300 rounded text-sm font-mono",
                  typ := "text",
                  controlled(
                    value <-- scalaVersionVar.signal,
                    onInput.mapToValue --> scalaVersionVar
                  )
                )
              ),
              div(
                cls := "mt-2",
                child <-- compileResultVar.signal.map {
                  case CodeEditor.CompileResult.NotStarted => emptyNode
                  case CodeEditor.CompileResult.Loading    =>
                    p(cls := "text-gray-500", "Compiling...")
                  case CodeEditor.CompileResult.Success(output) =>
                    div(
                      p(
                        cls := "text-green-600 font-semibold",
                        "Compilation successful"
                      ),
                      if (output.nonEmpty)
                        pre(
                          cls := "mt-1 p-2 text-sm bg-gray-50 border border-gray-300 rounded overflow-x-auto",
                          output
                        )
                      else emptyNode
                    )
                  case CodeEditor.CompileResult.Failed(errors) =>
                    div(
                      p(cls := "text-red-600 font-semibold", "Compilation failed"),
                      pre(
                        cls := "mt-1 p-2 text-sm text-red-800 bg-red-50 border border-red-300 rounded overflow-x-auto",
                        errors.mkString("\n")
                      )
                    )
                  case CodeEditor.CompileResult.UnknownFailure(ex) =>
                    p(
                      cls := "text-red-600",
                      s"Unexpected error: ${ex.getMessage}"
                    )
                }
              )
            )
        },
        viewer.component(convertedToSmithy4s)
      )
    )
  }
}
