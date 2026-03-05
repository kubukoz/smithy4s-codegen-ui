$version: "2"

namespace smithy4s_codegen.api

use alloy#simpleRestJson

@simpleRestJson
service SmithyCodeGenerationService {
    version: "1.0.0"
    operations: [
        HealthCheck
        GetConfiguration
        SmithyValidate
        Smithy4sConvert
        Smithy4sCompile
    ]
}

@http(method: "GET", uri: "/health", code: 200)
@readonly
operation HealthCheck {
    output := {
        @required
        message: String
    }
}

@http(method: "GET", uri: "/configuration", code: 200)
@readonly
operation GetConfiguration {
    output := {
        @required
        entries: DependencyEntries
    }
}

@http(method: "POST", uri: "/smithy/validate", code: 200)
operation SmithyValidate {
    input := with [SmithyCodegenInput] {}
    errors: [
        InvalidSmithyContent
    ]
}

@error("client")
structure InvalidSmithyContent {
    @required
    errors: ErrorMessages
}

@length(min: 1)
list ErrorMessages {
    member: String
}

@http(method: "POST", uri: "/smithy4s/convert", code: 200)
operation Smithy4sConvert {
    input := with [SmithyCodegenInput] {}

    output := {
        @required
        generated: Smithy4sGeneratedContent
    }

    errors: [
        InvalidSmithyContent
    ]
}

string Path

string Content

map Smithy4sGeneratedContent {
    key: Path
    value: Content
}

@http(method: "POST", uri: "/smithy4s/compile", code: 200)
operation Smithy4sCompile {
    input := with [SmithyCodegenInput] {
        @documentation("Scala version to use for compilation. If omitted, uses the server's default.")
        scalaVersion: String
    }

    output := {
        @required
        output: String
    }

    errors: [
        InvalidSmithyContent
        CompileError
    ]
}

@error("client")
structure CompileError {
    @required
    errors: ErrorMessages
}

@mixin
structure SmithyCodegenInput {
    @required
    content: String

    @documentation("If omitted, use the server's default.")
    deps: Dependencies
}

string Dependency

/// Map from dependency artifact ID to its configuration
map Dependencies {
    key: Dependency
    value: DependencyConfig
}

structure DependencyConfig {
    @required
    version: String
}

structure Permalink {
    @required
    version: Integer
    @required
    code: String
    @required
    deps: Dependencies
}

string DependencyName

structure DependencyEntry {
    @required
    artifactId: Dependency
}

/// Map from dependency name to entry
map DependencyEntries {
    key: DependencyName
    value: DependencyEntry
}
