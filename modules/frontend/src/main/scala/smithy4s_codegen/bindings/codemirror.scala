package smithy4s_codegen.bindings

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

// @codemirror/state
@JSImport("@codemirror/state", "EditorState")
@js.native
object EditorState extends js.Object {
  def create(config: js.Object): CMEditorState = js.native
}

@js.native
trait CMEditorState extends js.Object {
  def doc: CMText = js.native
}

@js.native
trait CMText extends js.Object {
  override def toString(): String = js.native
}

// @codemirror/view
@JSImport("@codemirror/view", "EditorView")
@js.native
class EditorView(config: js.Object) extends js.Object {
  def state: CMEditorState = js.native
  def dispatch(tr: js.Object): Unit = js.native
  def destroy(): Unit = js.native
}

@JSImport("@codemirror/view", "EditorView")
@js.native
object EditorViewCompanion extends js.Object {
  val updateListener: CMFacet = js.native
  val editable: CMFacet = js.native
  val lineWrapping: js.Any = js.native
}

@js.native
trait CMFacet extends js.Object {
  def of(value: js.Any): js.Any = js.native
}

@JSImport("@codemirror/view", "keymap")
@js.native
object keymap extends js.Object {
  def of(bindings: js.Array[js.Object]): js.Any = js.native
}

@JSImport("@codemirror/view", "lineNumbers")
@js.native
object lineNumbersFn extends js.Object {
  def apply(): js.Any = js.native
}

@JSImport("@codemirror/view", "highlightActiveLineGutter")
@js.native
object highlightActiveLineGutterFn extends js.Object {
  def apply(): js.Any = js.native
}

@JSImport("@codemirror/view", "drawSelection")
@js.native
object drawSelectionFn extends js.Object {
  def apply(): js.Any = js.native
}

// @codemirror/autocomplete
@JSImport("@codemirror/autocomplete", "completionKeymap")
@js.native
object completionKeymap extends js.Array[js.Object]

// @codemirror/commands
@JSImport("@codemirror/commands", "defaultKeymap")
@js.native
object defaultKeymap extends js.Array[js.Object]

@JSImport("@codemirror/commands", "historyKeymap")
@js.native
object historyKeymap extends js.Array[js.Object]

@JSImport("@codemirror/commands", "history")
@js.native
object historyFn extends js.Object {
  def apply(): js.Any = js.native
}

// @codemirror/language
@JSImport("@codemirror/language", "indentOnInput")
@js.native
object indentOnInputFn extends js.Object {
  def apply(): js.Any = js.native
}

@JSImport("@codemirror/language", "defaultHighlightStyle")
@js.native
object defaultHighlightStyle extends js.Object

@JSImport("@codemirror/language", "syntaxHighlighting")
@js.native
object syntaxHighlightingFn extends js.Object {
  def apply(highlighter: js.Any): js.Any = js.native
}

// Language support from codemirror-langs.js
@JSImport("/codemirror-langs.js", "smithyLanguage")
@js.native
object smithyLanguage extends js.Object

@JSImport("/codemirror-langs.js", "scalaLanguage")
@js.native
object scalaLanguage extends js.Object

@JSImport("/codemirror-langs.js", "githubLight")
@js.native
object githubLight extends js.Object

@JSImport("/codemirror-langs.js", "smithyCompletion")
@js.native
object smithyCompletion extends js.Object

// ViewUpdate trait for the update listener callback
@js.native
trait CMViewUpdate extends js.Object {
  def docChanged: Boolean = js.native
  def state: CMEditorState = js.native
}
