package tapir.docs.openapi.schema

import tapir.Schema.SRef
import tapir.openapi.OpenAPI.ReferenceOr
import tapir.openapi.{Schema => OSchema, _}
import tapir.{Constraint, Schema => TSchema}

/**
  * Converts a tapir schema to an OpenAPI schema, using the given map to resolve references.
  */
private[schema] class TSchemaToOSchema(fullNameToKey: Map[String, SchemaKey]) {
  def apply(schema: TSchema): ReferenceOr[OSchema] = {
    schema match {
      case TSchema.SInteger(c) =>
        Right(OSchema(`type` = SchemaType.Integer, minimum = minimum(c), maximum = maximum(c), enum = enum(c)))
      case TSchema.SNumber(c) =>
        Right(OSchema(`type` = SchemaType.Number, minimum = minimum(c), maximum = maximum(c), enum = enum(c)))
      case TSchema.SBoolean =>
        Right(OSchema(`type` = SchemaType.Boolean))
      case TSchema.SString(c) =>
        Right(OSchema(`type` = SchemaType.String, pattern = pattern(c), minLength = minLength(c), maxLength = maxLength(c), enum = enum(c)))
      case TSchema.SObject(_, fields, required) =>
        Right(OSchema(`type` = SchemaType.Object, required = required.toList, properties = fields.map {
          case (fieldName, fieldSchema) =>
            fieldName -> apply(fieldSchema)
        }.toListMap))
      case TSchema.SArray(el, c) =>
        Right(
          OSchema(`type` = SchemaType.Array)
            .copy(
              items = Some(apply(el))
            )
            .copy(maxItems = maxItems(c), minItems = minItems(c)))
      case TSchema.SBinary(_) =>
        Right(OSchema(`type` = SchemaType.String, format = Some(SchemaFormat.Binary)))
      case TSchema.SDate =>
        Right(OSchema(`type` = SchemaType.String, format = Some(SchemaFormat.Date)))
      case TSchema.SDateTime =>
        Right(OSchema(`type` = SchemaType.String, format = Some(SchemaFormat.DateTime)))
      case SRef(fullName) =>
        Left(Reference("#/components/schemas/" + fullNameToKey(fullName)))
    }
  }

  private def minItems(c: List[Constraint[_]]) = {
    collectConstraint(c) { case Constraint.MinItems(v) => v }
  }

  private def maxItems(c: List[Constraint[_]]) = {
    collectConstraint(c) { case Constraint.MaxItems(v) => v }
  }

  private def pattern(c: List[Constraint[_]]) = {
    collectConstraint(c) { case Constraint.Pattern(v) => v.toString() }
  }

  private def maxLength(c: List[Constraint[_]]) = {
    collectConstraint(c) { case Constraint.MaxLength(v) => v }
  }

  private def minLength(c: List[Constraint[_]]) = {
    collectConstraint(c) { case Constraint.MinLength(v) => v }
  }

  private def enum(c: List[Constraint[_]]) = {
    collectConstraint(c) { case Constraint.Enum(v) => v.map(_.toString) }
  }

  private def maximum(c: List[Constraint[_]]) = {
    collectConstraint(c) { case Constraint.Maximum(v) => v.toString }
  }

  private def minimum(c: List[Constraint[_]]) = {
    collectConstraint(c) { case Constraint.Minimum(v) => v.toString }
  }

  private def collectConstraint[T](c: List[Constraint[_]])(collector: PartialFunction[Constraint[_], T]) = {
    c.collectFirst(collector)
  }
}
