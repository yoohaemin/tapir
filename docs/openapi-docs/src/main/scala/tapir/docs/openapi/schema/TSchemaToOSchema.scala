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
        Right(OSchema(SchemaType.Integer).copy(minimum = c.collectFirst { case Constraint.Minimum(v: Int) => v }))
      case s @ TSchema.SNumber(_) =>
        Right(OSchema(SchemaType.Number))
      case TSchema.SBoolean =>
        Right(OSchema(SchemaType.Boolean))
      case TSchema.SString(_) =>
        Right(OSchema(SchemaType.String))
      case TSchema.SObject(_, fields, required) =>
        Right(
          OSchema(SchemaType.Object).copy(
            required = required.toList,
            properties = fields.map {
              case (fieldName, fieldSchema) =>
                fieldName -> apply(fieldSchema)
            }.toMap
          ))
      case TSchema.SArray(el, _) =>
        Right(
          OSchema(SchemaType.Array).copy(
            items = Some(apply(el))
          ))
      case TSchema.SBinary(_) =>
        Right(OSchema(SchemaType.String).copy(format = Some(SchemaFormat.Binary)))
      case SRef(fullName) =>
        Left(Reference("#/components/schemas/" + fullNameToKey(fullName)))
    }
  }
}
