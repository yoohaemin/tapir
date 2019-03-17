package tapir
import scala.collection.TraversableLike

sealed trait Schema {
  def show: String
}

sealed trait Constraint
object Constraint {
  case class Minimum[T](value: T) extends Constraint
  case class Maximum[T](value: T) extends Constraint
  case class Pattern(value: String) extends Constraint
  case class MaxLength(value: Int) extends Constraint
  case class MinLength(value: Int) extends Constraint
  case class MinItems(value: Int) extends Constraint
  case class MaxItems(value: Int) extends Constraint
  case class Enum[T](values: List[T]) extends Constraint
}

trait CanConstraint[T, U]
object CanConstraint {
  implicit object CanPatternString extends CanConstraint[Schema.SString, Constraint.Pattern]

  implicit object CanMinimumInt extends CanConstraint[Schema.SInteger, Constraint.Minimum[Int]]
  implicit object CanMinimumFloat extends CanConstraint[Schema.SNumber, Constraint.Minimum[Float]]
  implicit object CanMinimumDouble extends CanConstraint[Schema.SNumber, Constraint.Minimum[Double]]

  implicit object CanMaximumInt extends CanConstraint[Schema.SInteger, Constraint.Maximum[Int]]
  implicit object CanMaximumFloat extends CanConstraint[Schema.SNumber, Constraint.Maximum[Float]]
  implicit object CanMaximumDouble extends CanConstraint[Schema.SNumber, Constraint.Maximum[Double]]

  implicit object CanMinLength extends CanConstraint[Schema.SString, Constraint.MinLength]
  implicit object CanMaxLength extends CanConstraint[Schema.SString, Constraint.MaxLength]

  implicit object CanMinItems extends CanConstraint[Schema.SArray, Constraint.MinItems]
  implicit object CanMaxItems extends CanConstraint[Schema.SArray, Constraint.MaxItems]

  implicit object CanEnumInt extends CanConstraint[Schema.SInteger, Constraint.Enum[Int]]
  implicit object CanEnumString extends CanConstraint[Schema.SString, Constraint.Enum[String]]
}

object Schema {
  case class SString(constraints: List[Constraint] = List()) extends Schema {
    def show: String = "string"
  }
  case class SInteger(constraints: List[Constraint] = List()) extends Schema {
    def show: String = "integer"
  }
  case class SNumber(constraints: List[Constraint] = List()) extends Schema {
    def show: String = "number"
  }
  case object SBoolean extends Schema {
    def show: String = "boolean"
  }
  case class SObject(info: SObjectInfo, fields: Iterable[(String, Schema)], required: Iterable[String]) extends Schema {
    def show: String = s"object(${fields.map(f => s"${f._1}->${f._2.show}").mkString(",")};required:${required.mkString(",")})"
  }
  case class SArray(element: Schema, constraints: List[Constraint] = List()) extends Schema {
    def show: String = s"array(${element.show})"
  }
  case class SBinary(constraints: List[Constraint] = List()) extends Schema {
    def show: String = "binary"
  }

  case class SRef(fullName: String) extends Schema {
    def show: String = s"ref($fullName)"
  }

  case class SObjectInfo(shortName: String, fullName: String)
}
