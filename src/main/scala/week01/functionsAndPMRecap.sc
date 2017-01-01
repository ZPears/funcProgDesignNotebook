abstract class JSON

case class JSeq (elems: List[JSON])             extends JSON
case class JObj (bindings: Map[String, JSON])   extends JSON
case class JNum (num: Double)                   extends JSON
case class JStr (str: String)                   extends JSON
case class JBool (b: Boolean)                   extends JSON
case object JNull                               extends JSON

val data = JObj(Map(
  "firstName" -> JStr("John"),
  "lastName" -> JStr("Smith"),
  "address" -> JObj(Map(
    "streetAddress" -> JStr("21 2nd Street"),
    "state" -> JStr("NY"),
    "postalCode" -> JNum(10021)
  )),
  "phoneNumbers" -> JSeq(List(
    JObj(Map(
      "type" -> JStr("home"), "number" -> JStr("212 555-1234")
    )),
    JObj(Map(
      "type" -> JStr("fax"), "number" -> JStr("646 555-4567")
    ))
  ))
))

def show(json: JSON): String = json match {
  case JSeq(elems) =>
    "[" + (elems map show mkString ", ") + "]"
  case JObj(bindings) =>
    "{ " + (
      bindings.map{ case (k,v) => "\"" + k + "\": " + show(v) } mkString ", "
      ) + " }"
  case JNum(num) => num.toString
  case JStr(str) => '\"' + str + '\"'
  case JBool(b) => b.toString
  case JNull => "null"
}

show(data)

// Partial functions

val f: PartialFunction[String, String] = {case "ping" => "pong"}

f.isDefinedAt("ping")
f.isDefinedAt("pang")

// how to throw an exception anyway - isdefined at only protects you
// against the outermost pattern match in the function

val g: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: rest =>
    rest match {
      case Nil => "two"
    }
}

// is true, any yet...
g.isDefinedAt(List(1,2,3))
// returns a match error in the inner PM
g.apply(List(1,2,3))