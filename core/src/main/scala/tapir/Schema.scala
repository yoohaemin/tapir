package tapir

sealed trait Schema {
  def show: String
}

object Schema {
  case class SString(constraints: List[Constraint[String]] = List()) extends Schema {
    def show: String = "string"
  }
  object SString {
    def apply(constraints: Constraint[String]*): SString = SString(constraints.toList)
  }

  case class SInteger(constraints: List[Constraint[Int]] = List()) extends Schema {
    def show: String = "integer"
  }
  object SInteger {
    def apply(constraints: Constraint[Int]*): SInteger = SInteger(constraints.toList)
  }

  case class SNumber[T: Numeric](constraints: List[Constraint[T]] = List()) extends Schema {
    def show: String = "number"
  }
  object SNumber {
    def apply[T: Numeric](constraints: Constraint[T]*): SNumber[T] = SNumber(constraints.toList)
  }

  case object SBoolean extends Schema {
    def show: String = "boolean"
  }

  case class SObject(info: SObjectInfo, fields: Iterable[(String, Schema)], required: Iterable[String]) extends Schema {
    def show: String = s"object(${fields.map(f => s"${f._1}->${f._2.show}").mkString(",")};required:${required.mkString(",")})"
  }

  case class SArray(element: Schema, constraints: List[Constraint[Iterable[_]]] = List()) extends Schema {
    def show: String = s"array(${element.show})"
  }
  object SArray {
    def apply(element: Schema, constraints: Constraint[Iterable[_]]*): SArray = SArray(element, constraints.toList)
  }

  case class SBinary(constraints: List[Constraint[Iterable[_]]] = List()) extends Schema {
    def show: String = "binary"
  }
  object SBinary {
    def apply(constraints: Constraint[Iterable[_]]*): SBinary = SBinary(constraints.toList)
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
