package checklist
package std

import cats.implicits._
import checklist.std.Message._
import checklist.std.ior._
import org.scalatest._

class CatsSyntaxSpec extends FreeSpec with Matchers {
  import cats.data._

  case class Address(house: Int, street: String)

  def getField(name: String): Rule[IorMessages, Map[String, String], String] =
    rules.pure(_.get(name).map(Ior.right).getOrElse(Ior.left(errors(s"Field not found: $name"))))

  val parseAddress: Rule[IorMessages, Map[String, String], Address] =
    (
      getField("house").andThen(rules.parseInt).andThen(rules.gt(0)),
      getField("street").andThen(rules.nonEmpty[String])
    ).parMapN(Address.apply)

  def runTests(f: Map[String, String] => IorMessages[Address]): Unit = {
    "good data" in {
      val actual = parseAddress(Map(
        "house"  -> "29",
        "street" -> "Acacia Road"
      ))
      val expected = Ior.right(Address(29, "Acacia Road"))
      actual should be(expected)
    }

    "empty data" in {
      val actual   = parseAddress(Map.empty[String, String])
      val expected = Ior.left(errors("Field not found: house", "Field not found: street"))
      actual should be(expected)
    }

    "bad fields" in {
      val actual   = parseAddress(Map("house" -> "-1", "street" -> ""))
      val expected = Ior.both(errors("Must be greater than 0", "Must not be empty"), Address(-1, ""))
      actual should be(expected)
    }
  }

  runTests(parseAddress.apply)
}
