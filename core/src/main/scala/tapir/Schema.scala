package tapir

sealed trait Schema {
  def show: String
}

object Schema {
  case class SString(constraints: List[SafeConstraint[Constraint, SString]] = List()) extends Schema {
    def show: String = "string"
  }
  object SString {
    def apply(constraints: SafeConstraint[Constraint, SString]*): SString = SString(constraints.toList)
  }

  case class SInteger(constraints: List[SafeConstraint[Constraint, SInteger]] = List()) extends Schema {
    def show: String = "integer"
  }
  object SInteger {
    def apply(constraints: SafeConstraint[Constraint, SInteger]*): SInteger = SInteger(constraints.toList)
  }

  case class SNumber(constraints: List[SafeConstraint[Constraint, SNumber]] = List()) extends Schema {
    def show: String = "number"
  }
  object SNumber {
    def apply(constraints: SafeConstraint[Constraint, SNumber]*): SNumber = SNumber(constraints.toList)
  }

  case object SBoolean extends Schema {
    def show: String = "boolean"
  }

  case class SObject(info: SObjectInfo, fields: Iterable[(String, Schema)], required: Iterable[String]) extends Schema {
    def show: String = s"object(${fields.map(f => s"${f._1}->${f._2.show}").mkString(",")};required:${required.mkString(",")})"
  }

  case class SArray(element: Schema, constraints: List[SafeConstraint[Constraint, SArray]] = List()) extends Schema {
    def show: String = s"array(${element.show})"
  }
  object SArray {
    def apply(element: Schema, constraints: SafeConstraint[Constraint, SArray]*): SArray = SArray(element, constraints.toList)
  }

  case class SBinary(constraints: List[SafeConstraint[Constraint, SBinary]] = List()) extends Schema {
    def show: String = "binary"
  }
  object SBinary {
    def apply(constraints: SafeConstraint[Constraint, SBinary]*): SBinary = SBinary(constraints.toList)
  }

  case class SRef(fullName: String) extends Schema {
    def show: String = s"ref($fullName)"
  }

  case class SObjectInfo(shortName: String, fullName: String)
}
