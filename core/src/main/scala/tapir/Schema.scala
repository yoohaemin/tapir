package tapir

sealed trait Schema {
  def show: String
}

object Schema {
  case class SString(constraints: List[UnsafeConstraint[Constraint, SString]] = List()) extends Schema {
    def show: String = "string"
  }
  object SString {
    def apply(constraints: UnsafeConstraint[Constraint, SString]*): SString = SString(constraints.toList)
  }

  case class SInteger(constraints: List[UnsafeConstraint[Constraint, SInteger]] = List()) extends Schema {
    def show: String = "integer"
  }
  object SInteger {
    def apply(constraints: UnsafeConstraint[Constraint, SInteger]*): SInteger = SInteger(constraints.toList)
  }

  case class SNumber(constraints: List[UnsafeConstraint[Constraint, SNumber]] = List()) extends Schema {
    def show: String = "number"
  }
  object SNumber {
    def apply(constraints: UnsafeConstraint[Constraint, SNumber]*): SNumber = SNumber(constraints.toList)
  }

  case object SBoolean extends Schema {
    def show: String = "boolean"
  }

  case class SObject(info: SObjectInfo, fields: Iterable[(String, Schema)], required: Iterable[String]) extends Schema {
    def show: String = s"object(${fields.map(f => s"${f._1}->${f._2.show}").mkString(",")};required:${required.mkString(",")})"
  }

  case class SArray(element: Schema, constraints: List[UnsafeConstraint[Constraint, SArray]] = List()) extends Schema {
    def show: String = s"array(${element.show})"
  }
  object SArray {
    def apply(element: Schema, constraints: UnsafeConstraint[Constraint, SArray]*): SArray = SArray(element, constraints.toList)
  }

  case class SBinary(constraints: List[UnsafeConstraint[Constraint, SBinary]] = List()) extends Schema {
    def show: String = "binary"
  }
  object SBinary {
    def apply(constraints: UnsafeConstraint[Constraint, SBinary]*): SBinary = SBinary(constraints.toList)
  }

  case object SDate extends Schema {
    def show: String = "date"
  }
  case object SDateTime extends Schema {
    def show: String = "date-time"
  }

  case class SRef(fullName: String) extends Schema {
    def show: String = s"ref($fullName)"
  }

  case class SObjectInfo(shortName: String, fullName: String)
}
