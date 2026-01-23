addSbtPlugin(
  "com.disneystreaming.smithy4s" % "smithy4s-sbt-codegen" % "0.18.46"
)
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.11.7")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.20.2")
addSbtPlugin("io.spray" % "sbt-revolver" % "0.10.0")
addSbtPlugin("com.github.sbt" % "sbt-dynver" % "5.1.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")

addSbtPlugin("org.typelevel" % "sbt-typelevel-mergify" % "0.8.0")
