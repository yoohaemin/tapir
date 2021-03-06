package tapir.examples

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.softwaremill.sttp._
import tapir._
import tapir.server.akkahttp._
import tapir.json.circe._
import io.circe.generic.auto._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object ErrorOutputsAkkaServer extends App {
  // the endpoint description
  case class Result(result: Int)

  val errorOrJson: Endpoint[Int, String, Result, Nothing] =
    endpoint.get
      .in(query[Int]("amount"))
      .out(jsonBody[Result])
      .errorOut(stringBody)

  // converting an endpoint to a route
  val errorOrJsonRoute: Route = errorOrJson.toRoute {
    case x if x < 0 => Future.successful(Left("Invalid parameter, smaller than 0!"))
    case x          => Future.successful(Right(Result(x * 2)))
  }

  // starting the server
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  try {
    Await.result(Http().bindAndHandle(errorOrJsonRoute, "localhost", 8080), 1.minute)

    // testing
    implicit val backend: SttpBackend[Id, Nothing] = HttpURLConnectionBackend()

    val result1: Either[String, String] = sttp.get(uri"http://localhost:8080?amount=-5").send().body
    println("Got result (1): " + result1)
    assert(result1 == Left("Invalid parameter, smaller than 0!"))

    val result2: Either[String, String] = sttp.get(uri"http://localhost:8080?amount=21").send().body
    println("Got result (2): " + result2)
    assert(result2 == Right("""{"result":42}"""))
  } finally {
    // cleanup
    actorSystem.terminate()
  }
}
