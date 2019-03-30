package tapir.docs.openapi.schema

import tapir.Schema.SRef
import tapir.openapi.OpenAPI.ReferenceOr
import tapir.openapi.{Schema => OSchema, _}
import tapir.{Constraint, UnsafeConstraint, Schema => TSchema}

/**
  * Converts a tapir schema to an OpenAPI schema, using the given map to resolve references.
  */
private[schema] class TSchemaToOSchema(fullNameToKey: Map[String, SchemaKey]) {
  def apply(schema: TSchema): ReferenceOr[OSchema] = {
    schema match {
      case TSchema.SInteger(c) =>
        Right(
          OSchema(SchemaType.Integer).copy(
            minimum = minimum(c),
            maximum = maximum(c),
            enum = enum(c)
          ))
      case TSchema.SNumber(c) =>
        Right(
          OSchema(SchemaType.Number).copy(
            minimum = minimum(c),
            maximum = maximum(c),
            enum = enum(c)
          ))
      case TSchema.SBoolean =>
        Right(OSchema(SchemaType.Boolean))
      case TSchema.SString(c) =>
        Right(
          OSchema(SchemaType.String).copy(
            pattern = pattern(c),
            minLength = minLength(c),
            maxLength = maxLength(c),
            enum = enum(c)
          ))
      case TSchema.SObject(_, fields, required) =>
        Right(
          OSchema(SchemaType.Object).copy(
            required = required.toList,
            properties = fields.map {
              case (fieldName, fieldSchema) =>
                fieldName -> apply(fieldSchema)
            }.toListMap
          ))
      case TSchema.SArray(el, c) =>
        Right(
          OSchema(SchemaType.Array)
            .copy(
              items = Some(apply(el))
            )
            .copy(maxItems = maxItems(c), minItems = minItems(c)))
      case TSchema.SBinary(_) =>
        Right(OSchema(SchemaType.String).copy(format = Some(SchemaFormat.Binary)))
      case TSchema.SDate =>
        Right(OSchema(SchemaType.String).copy(format = Some(SchemaFormat.Date)))
      case TSchema.SDateTime =>
        Right(OSchema(SchemaType.String).copy(format = Some(SchemaFormat.DateTime)))
      case SRef(fullName) =>
        Left(Reference("#/components/schemas/" + fullNameToKey(fullName)))
    }
  }

  private def minItems(c: List[UnsafeConstraint[Constraint, TSchema.SArray]]) = {
    c.map(_.constraint).collectFirst { case Constraint.MinItems(v) => v }
  }

  private def maxItems(c: List[UnsafeConstraint[Constraint, TSchema.SArray]]) = {
    c.map(_.constraint).collectFirst { case Constraint.MaxItems(v) => v }
  }

  private def pattern(c: List[UnsafeConstraint[Constraint, TSchema.SString]]) = {
    c.map(_.constraint).collectFirst { case Constraint.Pattern(v) => v.toString() }
  }

  private def maxLength(c: List[UnsafeConstraint[Constraint, TSchema.SString]]) = {
    c.map(_.constraint).collectFirst { case Constraint.MaxLength(v) => v }
  }

  private def minLength(c: List[UnsafeConstraint[Constraint, TSchema.SString]]) = {
    c.map(_.constraint).collectFirst { case Constraint.MinLength(v) => v }
  }

  private def enum(c: List[UnsafeConstraint[Constraint, _]]) = {
    c.map(_.constraint).collectFirst { case Constraint.Enum(v) => v.map(_.toString) }
  }

  private def maximum(c: List[UnsafeConstraint[Constraint, _]]) = {
    c.map(_.constraint).collectFirst { case Constraint.Maximum(v) => v.toString }
  }

  private def minimum(c: List[UnsafeConstraint[Constraint, _]]) = {
    c.map(_.constraint).collectFirst { case Constraint.Minimum(v) => v.toString }
  }
}
