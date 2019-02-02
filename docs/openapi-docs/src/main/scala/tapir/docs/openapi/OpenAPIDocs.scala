package tapir.docs.openapi
import tapir.openapi.OpenAPI
import tapir.{Endpoint, Server}

trait OpenAPIDocs {
  implicit class RichOpenAPIEndpoint[I, E, O, S](e: Endpoint[I, E, O, S]) {
    def toOpenAPI(title: String, version: String, servers: List[Server] = List.empty)(implicit options: OpenAPIDocsOptions): OpenAPI =
      EndpointToOpenAPIDocs.toOpenAPI(title, version, Seq(e), servers, options)
  }

  implicit class RichOpenAPIEndpoints(es: Iterable[Endpoint[_, _, _, _]]) {
    def toOpenAPI(title: String, version: String, servers: List[Server] = List.empty)(implicit options: OpenAPIDocsOptions): OpenAPI =
      EndpointToOpenAPIDocs.toOpenAPI(title, version, es, servers, options)
  }
}
