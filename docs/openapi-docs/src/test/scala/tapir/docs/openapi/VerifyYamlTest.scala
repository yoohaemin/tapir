package tapir.docs.openapi

import com.softwaremill.tagging.@@
import io.circe.generic.auto._
import io.circe.{Decoder, Encoder}
import org.scalatest.{FunSuite, Matchers}
import tapir.support.tagging._
import tapir.Codec._
import tapir.Constraint._
import tapir.Schema.SArray
import tapir._
import tapir.docs.openapi.VerifyYamlTest.Color
import tapir.json.circe._
import tapir.model.{Method, StatusCodes}
import tapir.openapi.circe.yaml._
import tapir.openapi.{Contact, Info, License}
import tapir.tests._

import scala.io.Source

class VerifyYamlTest extends FunSuite with Matchers {

  val all_the_way: Endpoint[(FruitAmount, String), Unit, (FruitAmount, Int), Nothing] = endpoint
    .in(("fruit" / path[String] / "amount" / path[Int]).mapTo(FruitAmount))
    .in(query[String]("color"))
    .out(jsonBody[FruitAmount])
    .out(header[Int]("X-Role"))

  test("should match the expected yaml") {
    val expectedYaml = loadYaml("expected.yml")

    val actualYaml = List(in_query_query_out_string, all_the_way).toOpenAPI(Info("Fruits", "1.0")).toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  val endpoint_wit_recursive_structure: Endpoint[Unit, Unit, F1, Nothing] = endpoint
    .out(jsonBody[F1])

  test("should match the expected yaml when schema is recursive") {
    val expectedYaml = loadYaml("expected_recursive.yml")

    val actualYaml = endpoint_wit_recursive_structure.toOpenAPI(Info("Fruits", "1.0")).toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should use custom operationId generator") {
    def customOperationIdGenerator(pc: Vector[String], m: Method) = pc.map(_.toUpperCase).mkString("", "+", "-") + m.m.toUpperCase
    val options = OpenAPIDocsOptions.default.copy(customOperationIdGenerator)
    val expectedYaml = loadYaml("expected_custom_operation_id.yml")

    val actualYaml = in_query_query_out_string
      .in("add")
      .in("path")
      .toOpenAPI(Info("Fruits", "1.0"))(options)
      .toYaml
    noIndentation(actualYaml) shouldBe expectedYaml
  }

  val streaming_endpoint: Endpoint[Vector[Byte], Unit, Vector[Byte], Vector[Byte]] = endpoint
    .in(streamBody[Vector[Byte]](schemaFor[String], MediaType.TextPlain()))
    .out(streamBody[Vector[Byte]](Schema.SBinary(), MediaType.OctetStream()))

  test("should match the expected yaml for streaming endpoints") {
    val expectedYaml = loadYaml("expected_streaming.yml")

    val actualYaml = streaming_endpoint.toOpenAPI(Info("Fruits", "1.0")).toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should support tags") {
    val userTaggedEndpointShow = endpoint.tag("user").in("user" / "show").get.out(plainBody[String])
    val userTaggedEdnpointSearch = endpoint.tags(List("user", "admin")).in("user" / "search").get.out(plainBody[String])
    val adminTaggedEndpointAdd = endpoint.tag("admin").in("admin" / "add").get.out(plainBody[String])

    val expectedYaml = loadYaml("expected_tags.yml")

    val actualYaml = List(userTaggedEndpointShow, userTaggedEdnpointSearch, adminTaggedEndpointAdd).toOpenAPI(Info("Fruits", "1.0")).toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should match the expected yaml for general info") {
    val expectedYaml = loadYaml("expected_general_info.yml")

    val api = Info(
      "Fruits",
      "1.0",
      description = Some("Fruits are awesome"),
      termsOfService = Some("our.terms.of.service"),
      contact = Some(Contact(Some("Author"), Some("tapir@softwaremill.com"), Some("tapir.io"))),
      license = Some(License("MIT", Some("mit.license")))
    )

    val actualYaml = in_query_query_out_string.toOpenAPI(api).toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should support multipart") {
    val expectedYaml = loadYaml("expected_multipart.yml")

    val actualYaml = List(in_file_multipart_out_multipart).toOpenAPI("Fruits", "1.0").toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should support authentication") {
    val expectedYaml = loadYaml("expected_auth.yml")

    val e1 = endpoint.in(auth.bearer).in("api1" / path[String]).out(stringBody)
    val e2 = endpoint.in(auth.bearer).in("api2" / path[String]).out(stringBody)
    val e3 = endpoint.in(auth.apiKey(header[String]("apikey"))).in("api3" / path[String]).out(stringBody)

    val actualYaml = List(e1, e2, e3).toOpenAPI(Info("Fruits", "1.0")).toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should support empty bodies") {
    val expectedYaml = loadYaml("expected_empty.yml")

    val actualYaml = List(endpoint).toOpenAPI(Info("Fruits", "1.0")).toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should support multiple status codes") {
    // given
    val expectedYaml = loadYaml("expected_status_codes.yml")

    // work-around for #10: unsupported sealed trait families
    implicit val schemaForErrorInfo: SchemaFor[ErrorInfo] = new SchemaFor[ErrorInfo] {
      override def schema: Schema = Schema.SObject(Schema.SObjectInfo("ErrorInfo", "ErrorInfo"), Nil, Nil)
    }

    val e = endpoint.errorOut(
      statusFrom(
        jsonBody[ErrorInfo],
        StatusCodes.BadRequest,
        whenClass[ErrorInfo.NotFound] -> StatusCodes.NotFound,
        whenClass[ErrorInfo.Unauthorized] -> StatusCodes.Unauthorized
      ).defaultSchema(schemaFor[ErrorInfo.Unknown]))

    // when
    val actualYaml = List(e).toOpenAPI(Info("Fruits", "1.0")).toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    // then
    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should keep the order of multiple endpoints") {
    val expectedYaml = loadYaml("expected_multiple.yml")

    val actualYaml = List(endpoint.in("p1"), endpoint.in("p3"), endpoint.in("p2"), endpoint.in("p5"), endpoint.in("p4"))
      .toOpenAPI(Info("Fruits", "1.0"))
      .toYaml
    println(actualYaml)
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should support constraints for tagged types in input") {
    val expectedYaml = loadYaml("expected_constraints_custom_input.yml")
    implicit val schemaForColor: SchemaFor[Int @@ Color] = SchemaFor(Schema.SInteger(Constraint.Minimum(1)))

    val e = endpoint
      .in(query[Int @@ Color]("color"))
      .out(stringBody)
    val i = List(e).toOpenAPI(Info("Fruits", "1.0"))
    val actualYaml = i.toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should support constraints for value types in input") {
    val expectedYaml = loadYaml("expected_constraints_custom_input.yml")
    implicit val schemaForWrapper: SchemaFor[Wrapper] = SchemaFor(Schema.SInteger(Constraint.Minimum(1)))
    implicit val wrapperEncoder: Encoder[Wrapper] = Encoder.encodeInt.contramap(_.un)
    implicit val wrapperDecoder: Decoder[Wrapper] = Decoder.decodeInt.map(c => new Wrapper(c))
    implicit def plainCodecForWrapper(implicit uc: PlainCodec[Int], sf: SchemaFor[Wrapper]): Codec[Wrapper, MediaType.TextPlain, String] =
      uc.map(c => new Wrapper(c))(_.un).schema(sf.schema)

    val e = endpoint
      .in(query[Wrapper]("color"))
      .out(stringBody)
    val i = List(e).toOpenAPI(Info("Fruits", "1.0"))
    val actualYaml = i.toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should support constraints for tagged types in output") {
    val expectedYaml = loadYaml("expected_constraints_taggged_output.yml")
    implicit val schemaForColor: SchemaFor[Int @@ Color] = SchemaFor(Schema.SInteger(Constraint.Minimum(1)))
    implicit val colorEncoder: Encoder[Int @@ Color] = Encoder.encodeInt.asInstanceOf[Encoder[Int @@ Color]]
    implicit val colorDecoder: Decoder[Int @@ Color] = Decoder.decodeInt.asInstanceOf[Decoder[Int @@ Color]]

    val e = endpoint
      .in(query[String]("color"))
      .out(jsonBody[FruitColor])
    val i = List(e).toOpenAPI(Info("Fruits", "1.0"))
    val actualYaml = i.toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should support constraints in builtin types in output through implicit overriding") {
    val expectedYaml = loadYaml("expected_constraints_known_output.yml")
    implicit def schemaForIterable[T: SchemaFor, C[_] <: Iterable[_]]: SchemaFor[C[T]] =
      SchemaFor(SArray(implicitly[SchemaFor[T]].schema, Constraint.MaxItems(10)))

    val e = endpoint
      .in(query[String]("color"))
      .out(jsonBody[F1])
    val i = List(e).toOpenAPI(Info("Fruits", "1.0"))
    val actualYaml = i.toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should support constraints in builtin types in input through implicit overriding") {
    val expectedYaml = loadYaml("expected_constraints_known_input.yml")
    implicit val schemaForInt: SchemaFor[Int] = SchemaFor(Schema.SInteger(Constraint.Minimum(1)))

    val e = endpoint
      .in(query[Int]("color"))
      .out(stringBody)
    val i = List(e).toOpenAPI(Info("Fruits", "1.0"))
    val actualYaml = i.toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)

    actualYamlNoIndent shouldBe expectedYaml
  }

  test("should support all type of constraints") {
    val expectedYaml = loadYaml("expected_all_constraints.yml")
    implicit def schemaForIterable[T: SchemaFor, C[_] <: Iterable[_]]: SchemaFor[C[T]] =
      SchemaFor(SArray(implicitly[SchemaFor[T]].schema, Constraint.MinItems(1), Constraint.MaxItems(10)))
    implicit def schemaForString: SchemaFor[String] =
      SchemaFor[String](
        Schema.SString(Constraint.Pattern("\\w".r), Constraint.MinLength(4), Constraint.MaxLength(20), Constraint.Enum("apple", "banana")))
    implicit val schemaForColor: SchemaFor[Int @@ Color] =
      SchemaFor(Schema.SInteger(Constraint.Minimum(1), Constraint.Maximum(255), Constraint.Enum(125, 200, 235)))
    implicit val schemaForDouble: SchemaFor[Double] =
      SchemaFor(Schema.SNumber(Constraint.Minimum(1.0), Constraint.Maximum(10.0), Constraint.Enum(1.0, 2.0, 3.0)))
    implicit val colorEncoder: Encoder[Int @@ Color] = Encoder.encodeInt.asInstanceOf[Encoder[Int @@ Color]]
    implicit val colorDecoder: Decoder[Int @@ Color] = Decoder.decodeInt.asInstanceOf[Decoder[Int @@ Color]]

    val e = endpoint
      .out(jsonBody[List[FruitColor]])

    val i = List(e).toOpenAPI(Info("Fruits", "1.0"))
    val actualYaml = i.toYaml
    val actualYamlNoIndent = noIndentation(actualYaml)
    println(actualYaml)
    actualYamlNoIndent shouldBe expectedYaml
  }

  private def loadYaml(fileName: String): String = {
    noIndentation(Source.fromResource(fileName).getLines().mkString("\n"))
  }

  private def noIndentation(s: String) = s.replaceAll("[ \t]", "").trim
}

object VerifyYamlTest {
  type Color = Int
}

case class F1(data: List[F1])
case class FruitColor(fruit: String, color: Int @@ Color, weight: Double)
class Wrapper(val un: Int) extends AnyVal

sealed trait ErrorInfo
object ErrorInfo {
  case class NotFound(what: String) extends ErrorInfo
  case class Unauthorized(realm: String) extends ErrorInfo
  case class Unknown(code: Int, msg: String) extends ErrorInfo
}
