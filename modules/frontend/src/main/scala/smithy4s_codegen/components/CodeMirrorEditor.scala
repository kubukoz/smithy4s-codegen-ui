package smithy4s_codegen.components

import com.raquo.laminar.api.L._
import scala.scalajs.js
import smithy4s_codegen.bindings._

object CodeMirrorEditor {

  /** Create a CodeMirror 6 editor element.
    *
    * @param contentSignal
    *   Signal providing the current text (used to push content into the editor
    *   from outside)
    * @param onChangeObserver
    *   Observer that receives text changes made inside the editor
    * @param readOnly
    *   If true, the editor is not editable
    */
  def apply(
      contentSignal: Signal[String],
      onChangeObserver: Observer[String],
      readOnly: Boolean = false,
      extraExtensions: Seq[js.Any] = Nil,
      contentSized: Boolean = false
  ): HtmlElement = {
    var viewRef: Option[EditorView] = None

    val changeListener = EditorViewCompanion.updateListener.of(
      js.Any.fromFunction1 { (update: CMViewUpdate) =>
        if (update.docChanged) {
          val text = update.state.doc.toString()
          onChangeObserver.onNext(text)
        }
      }
    )

    val editabilityExtension: js.Any =
      if (readOnly)
        EditorViewCompanion.editable.of(false.asInstanceOf[js.Any])
      else
        EditorViewCompanion.lineWrapping

    val extensions: js.Array[js.Any] = js.Array(
      lineNumbersFn(),
      highlightActiveLineGutterFn(),
      drawSelectionFn(),
      historyFn(),
      indentOnInputFn(),
      syntaxHighlightingFn(defaultHighlightStyle),
      keymap.of(defaultKeymap.concat(historyKeymap).concat(completionKeymap)),
      editabilityExtension,
      changeListener
    ).concat(githubLight.asInstanceOf[js.Array[js.Any]])
     .concat(js.Array(extraExtensions*))

    div(
      cls := (if (contentSized) "cm-wrapper-content-sized" else "cm-wrapper h-full overflow-hidden"),
      onMountCallback { ctx =>
        val state = EditorState.create(
          js.Dynamic
            .literal(
              doc = "",
              extensions = extensions
            )
            .asInstanceOf[js.Object]
        )
        val view = new EditorView(
          js.Dynamic
            .literal(
              state = state,
              parent = ctx.thisNode.ref
            )
            .asInstanceOf[js.Object]
        )
        viewRef = Some(view)
      },
      // Sync externally-driven content changes into the editor
      contentSignal --> { newContent =>
        viewRef.foreach { view =>
          val current = view.state.doc.toString()
          if (current != newContent) {
            view.dispatch(
              js.Dynamic
                .literal(
                  changes = js.Dynamic.literal(
                    from = 0,
                    to = current.length,
                    insert = newContent
                  )
                )
                .asInstanceOf[js.Object]
            )
          }
        }
      },
      onUnmountCallback { _ =>
        viewRef.foreach(_.destroy())
        viewRef = None
      }
    )
  }
}
