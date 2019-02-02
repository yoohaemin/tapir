package tapir.client.sttp

import com.softwaremill.sttp.Request
import tapir.typelevel.ParamsAsArgs
import tapir.{Endpoint, Server}

trait SttpClient {
  implicit class RichEndpoint[I, E, O, S](e: Endpoint[I, E, O, S]) {
    def toSttpRequest(server: Server)(implicit paramsAsArgs: ParamsAsArgs[I],
                                      clientOptions: SttpClientOptions): paramsAsArgs.FN[Request[Either[E, O], S]] =
      new EndpointToSttpClient(clientOptions).toSttpRequest(e, server)
  }
}
