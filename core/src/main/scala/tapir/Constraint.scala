package tapir

import scala.util.matching.Regex

sealed trait Constraint
object Constraint {
  case class Minimum[T](value: T) extends Constraint
  case class Maximum[T](value: T) extends Constraint
  case class Pattern(value: Regex) extends Constraint
  case class MaxLength(value: Int) extends Constraint
  case class MinLength(value: Int) extends Constraint
  case class MinItems(value: Int) extends Constraint
  case class MaxItems(value: Int) extends Constraint
  case class Enum[T](values: List[T]) extends Constraint
  object Enum {
    def apply[T](v: T*): Enum[T] = Enum(v.toList)
  }

  implicit def asUnsafeConstraint[C <: Constraint, S <: Schema](c: C)(implicit canConstraint: CanConstraint[S, C]) =
    UnsafeConstraint(c, canConstraint)
}

trait CanConstraint[S <: Schema, +C <: Constraint]
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
  implicit object CanEnumFloat extends CanConstraint[Schema.SNumber, Constraint.Enum[Float]]
  implicit object CanEnumDouble extends CanConstraint[Schema.SNumber, Constraint.Enum[Double]]

  implicit object CanEnumString extends CanConstraint[Schema.SString, Constraint.Enum[String]]
}

case class UnsafeConstraint[+C <: Constraint, S <: Schema](constraint: C, canConstraint: CanConstraint[S, C])
