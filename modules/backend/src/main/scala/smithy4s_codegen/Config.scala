package smithy4s_codegen

import ciris._
import fs2.io.file.{Path => FPath}
import cats.implicits._
import fs2.io.file.Files
import cats.effect.IO
import io.circe._, io.circe.generic.semiauto._

final class InvalidConfiguration(message: String)
    extends RuntimeException(message)

final case class SmithyClasspathEntry(
    artifactId: String,
    file: FPath
)

final case class SmithyClasspathConfig(
    entries: Map[String, SmithyClasspathEntry]
)
object SmithyClasspathConfig {
  val Empty: SmithyClasspathConfig = SmithyClasspathConfig(Map.empty)
}

private final case class JsonSmithyClasspathEntry(
    artifactId: String,
    file: String
)

private final case class JsonSmithyClasspathConfig(
    entries: Map[String, JsonSmithyClasspathEntry]
)

private object JsonSmithyClasspathConfig {
  implicit val entryDecoder: Decoder[JsonSmithyClasspathEntry] =
    deriveDecoder[JsonSmithyClasspathEntry]
  implicit val configDecoder: Decoder[JsonSmithyClasspathConfig] =
    deriveDecoder[JsonSmithyClasspathConfig]
}

final case class Config(
    smithyClasspathConfig: SmithyClasspathConfig
)

object Config {
  private def asFPath(_path: String) = {
    val path = FPath(_path)
    Files[IO]
      .exists(path)
      .ifM(
        path.pure[IO],
        IO.raiseError(
          new IllegalArgumentException(
            s"File at ${_path} does not exist."
          )
        )
      )
  }

  private def loadSmithyClasspathConfig(path: FPath) = {
    import JsonSmithyClasspathConfig.configDecoder

    Files[IO]
      .readUtf8(path)
      .compile
      .lastOrError
      .flatMap { payload =>
        parser.decode[JsonSmithyClasspathConfig](payload).liftTo[IO]
      }
      .flatMap { jsonConfig =>
        jsonConfig.entries.toList
          .traverse { case (name, entry) =>
            asFPath(entry.file).map { filePath =>
              name -> SmithyClasspathEntry(entry.artifactId, filePath)
            }
          }
          .map(_.toMap)
          .map(SmithyClasspathConfig.apply)
      }
  }

  val smithyClasspathConfig =
    env("SMITHY_CLASSPATH_CONFIG")
      .evalMap { p => asFPath(p).flatMap(loadSmithyClasspathConfig) }
      .option
      .map(_.getOrElse(SmithyClasspathConfig.Empty))

  val config = smithyClasspathConfig.map(Config.apply)

  val makeConfig = config.load[IO]

}
