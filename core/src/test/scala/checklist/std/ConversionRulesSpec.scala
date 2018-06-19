package checklist
package std

import cats.data.Ior
import checklist.std.ior._
import org.scalatest._

class ConversionRulesSpec extends FreeSpec with Matchers {
  "parseInt" in {
    val rule = rules.parseInt
    rule("abc")   should be(Ior.left(Message.errors("Must be a whole number")))
    rule("123")   should be(Ior.right(123))
    rule("123.4") should be(Ior.left(Message.errors("Must be a whole number")))
  }

  "parseDouble" in {
    val rule = rules.parseDouble
    rule("abc")   should be(Ior.left(Message.errors("Must be a number")))
    rule("123")   should be(Ior.right(123.0))
    rule("123.4") should be(Ior.right(123.4))
  }

  "trimString" in {
    val rule = rules.trimString
    rule("") should be(Ior.right(""))
    rule("foo") should be(Ior.right("foo"))
    rule(" foo ") should be(Ior.right("foo"))
  }
}
