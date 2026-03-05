package smithy4s_codegen.components

import com.raquo.laminar.api.L._
import smithy4s_codegen.api.Content
import smithy4s_codegen.api.Path
import smithy4s_codegen.components.CodeEditor.Smithy4sConversionResult

class CodeViewer() {
  def component(content: EventStream[CodeEditor.Smithy4sConversionResult]) = {
    val success: EventStream[List[(Path, Content)]] = content.collect {
      case Smithy4sConversionResult.Success(content) => content.toList
    }
    val fileAndContent: Signal[List[HtmlElement]] =
      success.split(_._1)(render)

    val icon = ResultIcon(content.map {
      case Smithy4sConversionResult.Loading    => ResultIcon.State.Loading
      case Smithy4sConversionResult.Success(_) => ResultIcon.State.Success
      case Smithy4sConversionResult.UnknownFailure(_) => ResultIcon.State.Failed
    })

    div(
      icon,
      div(
        children <-- fileAndContent
      )
    )
  }

  private def render(
      path: Path,
      initial: (Path, Content),
      signal: Signal[(Path, Content)]
  ): HtmlElement =
    div(
      cls := "mb-4",
      p(
        cls := "text-xs font-mono text-gray-500 mb-1 px-1",
        child.text <-- signal.map(_._1.value)
      ),
      div(
        cls := "rounded-lg border border-gray-300 overflow-hidden",
        CodeMirrorEditor(
          contentSignal = signal.map(_._2.value),
          onChangeObserver = Observer.empty,
          readOnly = true,
          extraExtensions = Seq(smithy4s_codegen.bindings.scalaLanguage),
          contentSized = true
        )
      )
    )
}
