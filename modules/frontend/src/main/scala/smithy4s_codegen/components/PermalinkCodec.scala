package smithy4s_codegen.components

import smithy4s_codegen.api.Dependency
import smithy4s_codegen.api.DependencyConfig
import smithy4s_codegen.bindings.lzstring

/** Pure encode/decode for permalink hashes. Does not touch the DOM.
  *
  * New format: the whole hash is a single lzstring-compressed JSON object
  * representing a [[smithy4s_codegen.api.Permalink]].
  *
  * Legacy format (read-only): #code=<lzstring>;dependencies=<comma-separated
  * artifact IDs>. Deps are restored with their default versions (extracted
  * from the artifact ID).
  */
object PermalinkCodec {
  private val currentVersion = 1

  def readOnce(): Option[EditorContent] =
    decode(org.scalajs.dom.window.location.hash)

  def encode(value: EditorContent): String = {
    import smithy4s.json.Json
    import smithy4s_codegen.api.Permalink
    val permalink = Permalink(
      version = currentVersion,
      code = value.code,
      deps = value.deps
    )
    val json = Json.writeBlob(permalink).toUTF8String
    "#" + lzstring.compressToEncodedURIComponent(json)
  }

  def decode(hash: String): Option[EditorContent] =
    decodeNew(hash).orElse(decodeLegacy(hash))

  private def decodeNew(hash: String): Option[EditorContent] = {
    import smithy4s.json.Json
    import smithy4s_codegen.api.Permalink
    if (!hash.startsWith("#")) None
    else {
      val compressed = hash.drop(1)
      Option(lzstring.decompressFromEncodedURIComponent(compressed))
        .flatMap { json =>
          Json
            .read(smithy4s.Blob(json))
            .toOption
            .map { (p: Permalink) => EditorContent(p.code, p.deps) }
        }
    }
  }

  // Legacy format: #code=<lzstring>;dependencies=<comma-separated artifact IDs>
  // Deps are restored with their default versions (extracted from the artifact ID).
  private def decodeLegacy(hash: String): Option[EditorContent] = {
    if (!hash.startsWith("#")) None
    else {
      val parts = hash.drop(1).split(";")

      val maybeCode = parts.collectFirst {
        case s if s.startsWith("code=") =>
          Option(
            lzstring.decompressFromEncodedURIComponent(s.drop("code=".length))
          )
      }.flatten

      val deps: Map[Dependency, DependencyConfig] =
        parts
          .collectFirst {
            case s if s.startsWith("dependencies=") =>
              s.drop("dependencies=".length)
                .split(",")
                .filter(_.nonEmpty)
                .map { artifactId =>
                  val version = artifactId.lastIndexOf(':') match {
                    case -1  => ""
                    case idx => artifactId.drop(idx + 1)
                  }
                  Dependency(artifactId) -> DependencyConfig(version)
                }
                .toMap
          }
          .getOrElse(Map.empty)

      maybeCode.map(code => EditorContent(code, deps))
    }
  }
}
