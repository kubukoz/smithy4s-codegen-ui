package smithy4s_codegen.components

import cats.effect.IO
import smithy4s_codegen.api.Dependency
import smithy4s_codegen.api.DependencyConfig
import weaver._

object PermalinkCodecTest extends SimpleIOSuite {

  private val sampleContent = EditorContent(
    code = """|$version: "2"
              |namespace example
              |structure Foo { bar: String }""".stripMargin,
    deps = Map(
      Dependency("com.example::foo:1.2.3") -> DependencyConfig("1.2.3"),
      Dependency("com.example::bar:4.5.6") -> DependencyConfig("9.0.0")
    )
  )

  test("new format: round-trips code and deps") {
    IO {
      val hash = PermalinkCodec.encode(sampleContent)
      val decoded = PermalinkCodec.decode(hash)
      expect(decoded == Some(sampleContent))
    }
  }

  test("new format: round-trips with empty deps") {
    IO {
      val content = EditorContent(code = "hello", deps = Map.empty)
      val decoded = PermalinkCodec.decode(PermalinkCodec.encode(content))
      expect(decoded == Some(content))
    }
  }

  test("new format: hash starts with #") {
    IO {
      val hash = PermalinkCodec.encode(sampleContent)
      expect(hash.startsWith("#"))
    }
  }

  test("new format: version override is preserved") {
    IO {
      val content = EditorContent(
        code = "code",
        deps =
          Map(Dependency("com.foo::bar:1.0.0") -> DependencyConfig("2.0.0"))
      )
      val decoded = PermalinkCodec.decode(PermalinkCodec.encode(content))
      val version = decoded
        .flatMap(_.deps.get(Dependency("com.foo::bar:1.0.0")))
        .map(_.version)
      expect(version == Some("2.0.0"))
    }
  }

  test("legacy format: restores code and deps with default versions") {
    IO {
      import smithy4s_codegen.bindings.lzstring
      // Reproduce what the old codec wrote
      val compressedCode = lzstring.compressToEncodedURIComponent("legacy code")
      val hash =
        s"#code=$compressedCode;dependencies=com.example::foo:1.2.3,com.example::bar:4.5.6"
      val decoded = PermalinkCodec.decode(hash)
      expect(decoded.map(_.code) == Some("legacy code")) and
        expect(
          decoded.map(_.deps.keySet) == Some(
            Set(
              Dependency("com.example::foo:1.2.3"),
              Dependency("com.example::bar:4.5.6")
            )
          )
        ) and
        expect(
          decoded
            .flatMap(_.deps.get(Dependency("com.example::foo:1.2.3")))
            .map(_.version) == Some("1.2.3")
        ) and
        expect(
          decoded
            .flatMap(_.deps.get(Dependency("com.example::bar:4.5.6")))
            .map(_.version) == Some("4.5.6")
        )
    }
  }

  test("legacy format: restores code with no dependencies part") {
    IO {
      import smithy4s_codegen.bindings.lzstring
      val compressedCode = lzstring.compressToEncodedURIComponent("just code")
      val hash = s"#code=$compressedCode"
      val decoded = PermalinkCodec.decode(hash)
      expect(decoded.map(_.code) == Some("just code")) and
        expect(decoded.map(_.deps) == Some(Map.empty))
    }
  }

  test("legacy format: real-world example") {
    IO {
      import smithy4s_codegen.bindings.lzstring
      val input =
        "code=CQNwpgTgzglg9gOwFwAIBEAmNAobCCGAtmFAA74DGYKMCpArgC65SMT0WP0TUAKkURCgDe2FCgACPAI70YPACZiUBYqgDKbWgHNsAXyA;dependencies=com.disneystreaming.smithy4s:smithy4s-protocol:0.18.49,com.disneystreaming.alloy:alloy-core:0.3.36"
      val decoded = PermalinkCodec.decode("#" + input)

      val expectedCode = lzstring.decompressFromEncodedURIComponent(
        "CQNwpgTgzglg9gOwFwAIBEAmNAobCCGAtmFAA74DGYKMCpArgC65SMT0WP0TUAKkURCgDe2FCgACPAI70YPACZiUBYqgDKbWgHNsAXyA"
      )

      expect(decoded.map(_.code) == Some(expectedCode)) and
        expect(
          decoded.map(_.deps) == Some(
            Map(
              Dependency(
                "com.disneystreaming.smithy4s:smithy4s-protocol:0.18.49"
              ) -> DependencyConfig("0.18.49"),
              Dependency(
                "com.disneystreaming.alloy:alloy-core:0.3.36"
              ) -> DependencyConfig("0.3.36")
            )
          )
        )
    }
  }

  test("decode returns None for empty string") {
    IO(expect(PermalinkCodec.decode("") == None))
  }

  test("decode returns None for garbage") {
    IO(expect(PermalinkCodec.decode("#notvalidatall!!!") == None))
  }
}
