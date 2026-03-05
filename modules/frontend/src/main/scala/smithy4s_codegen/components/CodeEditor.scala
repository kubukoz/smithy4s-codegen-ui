package smithy4s_codegen.components

import com.raquo.laminar.api.L._
import smithy4s_codegen.api.Content
import smithy4s_codegen.api.Path
import smithy4s_codegen.bindings.lzstring
import smithy4s_codegen.api.Dependency
import smithy4s_codegen.api.DependencyConfig
import smithy4s_codegen.api.GetConfigurationOutput

object CodeEditor {
  sealed trait ValidationResult
  object ValidationResult {
    case object Loading extends ValidationResult
    case class Success(editorContent: EditorContent) extends ValidationResult
    case class Failed(errors: List[String]) extends ValidationResult
    case class UnknownFailure(ex: Throwable) extends ValidationResult
  }

  sealed trait CompileResult
  object CompileResult {
    case object NotStarted extends CompileResult
    case object Loading extends CompileResult
    case class Success(output: String) extends CompileResult
    case class Failed(errors: List[String]) extends CompileResult
    case class UnknownFailure(ex: Throwable) extends CompileResult
  }

  sealed trait Smithy4sConversionResult
  object Smithy4sConversionResult {
    case object Loading extends Smithy4sConversionResult
    case class Success(content: Map[Path, Content])
        extends Smithy4sConversionResult
    case class UnknownFailure(ex: Throwable) extends Smithy4sConversionResult
  }
}
class CodeEditor(
    config: EventStream[Either[Throwable, GetConfigurationOutput]]
) {
  private val initial = """|$version: "2"
                           |
                           |namespace input
                           |
                           |structure Person {
                           |  @required
                           |  name: String
                           |}""".stripMargin
  val editorContent = Var(
    PermalinkCodec
      .readOnce()
      .getOrElse(EditorContent(initial, Map.empty))
  )

  // Store config as a Var for synchronous access
  private val configVar = Var[Option[GetConfigurationOutput]](None)

  val updatePermalinkCode = {
    val v = onInput.mapToValue.map(value => editorContent.now().copy(value))
    v --> editorContent
  }

  val updateValueFromPermalinkCode =
    value <-- editorContent.signal.map(_.code)

  // Extract default version from artifact ID (last colon-separated segment)
  private def defaultVersionOf(artifactId: String): String =
    artifactId.lastIndexOf(':') match {
      case -1  => ""
      case idx => artifactId.drop(idx + 1)
    }

  val dependenciesCheckboxes = {
    def displayIfHasErrors = styleAttr <-- config.map(res =>
      if (res.isLeft) "display: block"
      else "display: none"
    )
    val errors = div(
      displayIfHasErrors,
      child.text <-- config.collect { case Left(ex) =>
        "Unable to get available dependencies"
      }
    )
    val depsList = div(
      children <-- config.collect { case Right(cfg) =>
        val rows = cfg.entries.values.toList.map { entry =>
          val dep = entry.artifactId
          val defaultVersion = defaultVersionOf(dep.value)

          // Per-dep version Var, initialized from current state or default
          val versionVar: Var[String] = Var(
            editorContent.now().deps.get(dep).map(_.version).getOrElse(defaultVersion)
          )

          // When version changes and dep is checked, update the dep config
          val updateVersion = versionVar.signal.changes --> { newVersion =>
            editorContent.update { content =>
              if (content.deps.contains(dep))
                content.copy(deps = content.deps + (dep -> DependencyConfig(newVersion)))
              else content
            }
          }

          val toggleDep = onChange.mapToChecked --> { isChecked =>
            editorContent.update { content =>
              if (isChecked)
                content.copy(deps = content.deps + (dep -> DependencyConfig(versionVar.now())))
              else
                content.copy(deps = content.deps - dep)
            }
          }

          tr(
            updateVersion,
            td(
              cls := "pr-2 py-1",
              input(
                `type` := "checkbox",
                nameAttr := dep.value,
                idAttr := dep.value,
                toggleDep,
                checked <-- editorContent.signal.map(_.deps.contains(dep))
              )
            ),
            td(
              cls := "pr-4 py-1 font-mono text-sm",
              label(forId := dep.value, dep.value)
            ),
            td(
              cls := "py-1",
              input(
                cls := "px-2 py-1 border border-gray-300 rounded text-sm font-mono w-28",
                `type` := "text",
                controlled(
                  value <-- versionVar.signal,
                  onInput.mapToValue --> versionVar
                )
              )
            )
          )
        }
        List(
          fieldSet(
            legend(cls := "font-semibold mb-1", "Choose your dependencies"),
            table(
              thead(
                tr(
                  th(cls := "pr-2 py-1 text-left text-xs text-gray-500 font-medium", ""),
                  th(cls := "pr-4 py-1 text-left text-xs text-gray-500 font-medium", "Dependency"),
                  th(cls := "py-1 text-left text-xs text-gray-500 font-medium", "Version")
                )
              ),
              tbody(rows)
            )
          )
        )
      }
    )
    div(errors, depsList)
  }

  val sampleSelector = {
    val handleSampleChange = onChange.mapToValue --> { (selectedName: String) =>
      configVar.now().foreach { cfg =>
        // Build name to artifactId mapping
        val nameToArtifactId = cfg.entries.map { case (name, entry) =>
          name.value -> entry.artifactId.value
        }
        Samples.all
          .find(_.name == selectedName)
          .foreach { sample =>
            // Resolve sample dep names to artifact IDs
            val resolvedDeps = sample.deps.flatMap(nameToArtifactId.get)
            editorContent.set(
              EditorContent(
                code = sample.code,
                deps = resolvedDeps.map { artifactId =>
                  Dependency(artifactId) -> DependencyConfig(defaultVersionOf(artifactId))
                }.toMap
              )
            )
          }
      }
    }

    div(
      cls := "mb-2",
      label(
        forId := "sample-selector",
        "Load Sample: ",
        cls := "mr-2 text-sm font-medium text-gray-900"
      ),
      select(
        idAttr := "sample-selector",
        cls := "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 p-2",
        option(value := "", selected := true, "-- Select a sample --"),
        Samples.all.map { sample =>
          option(value := sample.name, sample.name)
        },
        handleSampleChange
      )
    )
  }

  val component =
    div(
      cls := "grow overflow-auto",
      config.collect { case Right(cfg) => Some(cfg) } --> configVar,
      sampleSelector,
      textArea(
        cls := "block p-2.5 w-full h-5/6 text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500 font-mono",
        onMountFocus,
        controlled(
          updateValueFromPermalinkCode,
          updatePermalinkCode
        )
      ),
      div(
        cls := "block p-2.5 w-full h-1/6",
        dependenciesCheckboxes
      )
    )

  def validationResult(
      validationResult: EventStream[CodeEditor.ValidationResult]
  ) = {
    def displayIfHasErrors = styleAttr <-- validationResult.map(res =>
      if (res.isInstanceOf[CodeEditor.ValidationResult.Failed]) "display: block"
      else "display: none"
    )
    val errors = div(
      displayIfHasErrors,
      child.text <-- validationResult.collect {
        case CodeEditor.ValidationResult.Failed(errors) => errors.mkString("\n")
      }
    )
    val icon = ResultIcon(validationResult.map {
      case CodeEditor.ValidationResult.Loading    => ResultIcon.State.Loading
      case CodeEditor.ValidationResult.Success(_) => ResultIcon.State.Success
      case CodeEditor.ValidationResult.Failed(_)  => ResultIcon.State.Failed
      case CodeEditor.ValidationResult.UnknownFailure(_) =>
        ResultIcon.State.Failed
    })
    (icon, errors)
  }

}

final case class EditorContent(code: String, deps: Map[Dependency, DependencyConfig])

/** Writes code to the URL hash and provides a stream of its decoded values.
  *
  * Encoding/decoding of code is handled internally.
  */
object PermalinkCodec {
  val hashTag = "#"
  val hashTagLength = hashTag.length()
  val hashPart = ";"

  def readOnce(): Option[EditorContent] =
    decode(org.scalajs.dom.window.location.hash)

  val read: EventStream[EditorContent] = windowEvents(_.onHashChange)
    .mapTo(org.scalajs.dom.window.location.hash)
    .map(decode(_))
    .collectSome

  def write(value: EditorContent): Unit =
    org.scalajs.dom.window.location.hash = encode(value)

  private class HashPartValue(partName: String) {
    private val partKey = s"$partName="
    def encode(value: String): String = s"$partKey$value"
    def unapply(value: String): Option[String] = {
      if (value.startsWith(partKey)) Some(value.drop(partKey.length()))
      else None
    }
  }
  private val codePart = new HashPartValue("code")
  private val depsPart = new HashPartValue("dependencies")

  // Encode deps as "artifactId|version" pairs joined by ","
  // artifactIds may contain ":" so we use "|" as the key/version separator
  private def encodeDeps(deps: Map[Dependency, DependencyConfig]): String =
    deps.map { case (dep, cfg) => s"${dep.value}|${cfg.version}" }.mkString(",")

  private def decodeDeps(s: String): Map[Dependency, DependencyConfig] =
    s.split(",")
      .filter(_.nonEmpty)
      .flatMap { entry =>
        entry.lastIndexOf('|') match {
          case -1  => None
          case idx =>
            val artifactId = entry.take(idx)
            val version = entry.drop(idx + 1)
            Some(Dependency(artifactId) -> DependencyConfig(version))
        }
      }
      .toMap

  private def encode(value: EditorContent): String = {
    val code =
      codePart.encode(lzstring.compressToEncodedURIComponent(value.code))
    val deps = depsPart.encode(encodeDeps(value.deps))
    val hash = List(code, deps).mkString(";")
    s"#$hash"
  }

  private def decode(hash: String): Option[EditorContent] = {
    if (hash.startsWith(hashTag)) {
      val hashParts = hash
        .drop(hashTagLength)
        .split(hashPart)

      val maybeCode = hashParts.collectFirst { case codePart(value) =>
        Option(lzstring.decompressFromEncodedURIComponent(value))
      }.flatten
      val deps =
        hashParts
          .collectFirst { case depsPart(value) => decodeDeps(value) }
          .getOrElse(Map.empty)
      maybeCode.map(code => EditorContent(code, deps))
    } else None
  }
}
