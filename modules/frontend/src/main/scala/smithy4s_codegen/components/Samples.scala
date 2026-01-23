package smithy4s_codegen.components

object Samples {
  case class Sample(name: String, code: String, deps: Set[String] = Set.empty)

  val all: List[Sample] = List(
    Sample(
      "Person",
      """|$version: "2"
         |
         |namespace input
         |
         |structure Person {
         |  @required
         |  name: String
         |}""".stripMargin
    ),
    Sample(
      "Simple Service",
      """|$version: "2"
         |
         |namespace example
         |
         |service HelloWorld {
         |  version: "1.0.0"
         |  operations: [SayHello]
         |}
         |
         |@http(method: "POST", uri: "/hello")
         |operation SayHello {
         |  input: HelloRequest
         |  output: HelloResponse
         |}
         |
         |structure HelloRequest {
         |  @required
         |  name: String
         |}
         |
         |structure HelloResponse {
         |  @required
         |  message: String
         |}""".stripMargin
    ),
    Sample(
      "Custom Refinements",
      """|$version: "2"
         |
         |namespace example.refinements
         |
         |use smithy4s.meta#refinement
         |
         |@trait(selector: "string")
         |structure emailFormat {}
         |
         |apply emailFormat @refinement(
         |  targetType: "example.types.Email"
         |  errorMessage: "Invalid email format"
         |)
         |
         |@emailFormat
         |string EmailAddress
         |
         |structure Person {
         |  @required email: EmailAddress
         |}""".stripMargin,
      Set("smithy4s-protocol")
    )
  )
}
