package smithy4s_codegen

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Dispatcher
import com.raquo.airstream.core.EventStream
import smithy4s.kinds.PolyFunction
import smithy4s_codegen.BuildInfo.baseUri
import smithy4s_codegen.api.SmithyCodeGenerationService
import smithy4s_fetch.SimpleRestJsonFetchClient

import scala.scalajs.js.Promise
import scala.scalajs.js.Thenable.Implicits._

object ApiBuilder {
  def build: Resource[IO, SmithyCodeGenerationService[EventStream]] =
    Dispatcher.sequential[IO].flatMap { dispatcher =>
      IO.executionContext.toResource.map { ec =>
        val fetchClient = SimpleRestJsonFetchClient(
          SmithyCodeGenerationService,
          s"${org.scalajs.dom.window.location.origin}$baseUri"
        ).make
        val ioClient = fetchClient.transform(new PolyFunction[Promise, IO] {
          def apply[A](fa: Promise[A]): IO[A] =
            IO.fromFuture(IO(fa.toFuture))
        })
        ioClient.transform(new PolyFunction[IO, EventStream] {
          def apply[A](fa: IO[A]): EventStream[A] =
            EventStream.fromFuture(dispatcher.unsafeToFuture(fa), emitOnce = true)(ec)
        })
      }
    }
}
