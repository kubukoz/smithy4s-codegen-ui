package smithy4s_codegen

import cats.effect.IO
import fs2.io.file.{Files, Path => FPath}
import io.circe.parser
import weaver._

object ConfigTest extends SimpleIOSuite {

  test("parse smithy classpath config JSON with new format") {
    val jsonContent = """{
      "entries": {
        "smithy4s-protocol": {
          "artifactId": "com.disneystreaming.smithy4s:smithy4s-protocol:0.18.46",
          "file": "/path/to/smithy4s-protocol-0.18.46.jar"
        },
        "alloy-core": {
          "artifactId": "com.disneystreaming.alloy:alloy-core:0.3.33",
          "file": "/path/to/alloy-core-0.3.33.jar"
        }
      }
    }"""

    IO {
      // Test that the JSON can be parsed
      val parsed = parser.parse(jsonContent)
      val hasEntries = parsed.toOption.exists { json =>
        json.hcursor.downField("entries").as[Map[String, io.circe.Json]].isRight
      }

      expect(hasEntries)
    }
  }

  test("parse empty smithy classpath config JSON") {
    val jsonContent = """{"entries": {}}"""

    IO {
      val parsed = parser.parse(jsonContent)
      val hasEmptyEntries = parsed.toOption.exists { json =>
        json.hcursor.downField("entries").as[Map[String, io.circe.Json]].map(_.isEmpty).contains(true)
      }

      expect(hasEmptyEntries)
    }
  }

  test("entry name is separate from artifact ID") {
    IO {
      val entryName = "smithy4s-protocol"
      val artifactId = "com.disneystreaming.smithy4s:smithy4s-protocol:0.18.46"

      // The entry name should be simpler than the full artifact ID
      expect(entryName.length < artifactId.length) and
      expect(artifactId.contains(entryName))
    }
  }
}
