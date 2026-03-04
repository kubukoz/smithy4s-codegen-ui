package smithy4s_codegen

import cats.effect.IO
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import com.raquo.airstream.core.EventStream
import org.http4s.Uri
import org.http4s.client.Client
import smithy4s.Service
import smithy4s.kinds.PolyFunction
import smithy4s.kinds.PolyFunction5
import smithy4s_codegen.BuildInfo.baseUri
import smithy4s_codegen.api.SmithyCodeGenerationService
import smithy4s_fetch.SimpleRestJsonFetchClient

import scala.scalajs.js.Promise

import util.chaining.*

object ApiBuilder {
  def build: Resource[IO, SmithyCodeGenerationService[EventStream]] =
    Resource.pure {
      val client = SimpleRestJsonFetchClient(
        SmithyCodeGenerationService, {
          val loc = org.scalajs.dom.window.location
          s"${loc.protocol}//${loc.host}" + baseUri
        }
      ).make

      client.transform(new PolyFunction[Promise, EventStream] {
        def apply[A](fa: Promise[A]): EventStream[A] =
          EventStream.fromJsPromise(fa, emitOnce = true)
      })
    }

}
