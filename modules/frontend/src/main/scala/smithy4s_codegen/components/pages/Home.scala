package smithy4s_codegen.components.pages

import com.raquo.airstream.ownership.ManualOwner
import com.raquo.laminar.api.L._
import smithy4s_codegen.api._
import smithy4s_codegen.components.CodeEditor
import smithy4s_codegen.components.CodeEditor.ValidationResult
import smithy4s_codegen.components.CodeViewer
import smithy4s_codegen.components.PermalinkCodec

object Home {
  def apply(
      api: SmithyCodeGenerationService[EventStream],
      config: EventStream[Either[Throwable, GetConfigurationOutput]]
  ) = {
    val editor = new CodeEditor(config)
    val viewer = new CodeViewer()

    locally {
      implicit val owner = new ManualOwner
      editor.editorContent.signal.foreach(PermalinkCodec.write)
    }

    val validate: EventStream[CodeEditor.ValidationResult] =
      editor.editorContent.signal
        .composeChanges(_.debounce(2000))
        .flatMapSwitch { content =>
          api
            .smithyValidate(content.code, Some(content.deps.toList))
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
              .smithy4sConvert(content.code, Some(content.deps.toList))
              .map(r =>
                CodeEditor.Smithy4sConversionResult.Success(r.generated)
              )
              .recover { ex =>
                Some(CodeEditor.Smithy4sConversionResult.UnknownFailure(ex))
              }
          }
      }

    val compileClicked = new EventBus[Unit]

    val compileResult: EventStream[CodeEditor.CompileResult] =
      EventStream.merge(
        compileClicked.events.mapTo(CodeEditor.CompileResult.Loading),
        compileClicked.events
          .withCurrentValueOf(editor.editorContent.signal)
          .flatMapSwitch { case (_, content) =>
            api
              .smithy4sCompile(content.code, Some(content.deps.toList))
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

    val (validateResultIcon, validateResultErrors) =
      editor.validationResult(validate)

    div(
      compileResult --> compileResultVar,
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
        viewer.component(convertedToSmithy4s),
        div(
          cls := "mt-4",
          button(
            cls := "px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 disabled:opacity-50",
            onClick.mapTo(()) --> compileClicked,
            disabled <-- compileResultVar.signal.map(_ ==
              CodeEditor.CompileResult.Loading),
            "Compile"
          ),
          div(
            cls := "mt-2",
            child <-- compileResultVar.signal.map {
              case CodeEditor.CompileResult.NotStarted => emptyNode
              case CodeEditor.CompileResult.Loading =>
                p(cls := "text-gray-500", "Compiling...")
              case CodeEditor.CompileResult.Success(output) =>
                div(
                  p(cls := "text-green-600 font-semibold", "Compilation successful"),
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
      )
    )
  }
}
