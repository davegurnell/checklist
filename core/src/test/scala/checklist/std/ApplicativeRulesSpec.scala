package checklist
package std

import cats.data.Ior
import cats.instances.list._
import checklist.std.ior._
import org.scalatest._

class ApplicativeRulesSpec extends FreeSpec with Matchers {
  "optional" in {
    val rule = rules.optional(rules.parseInt)
    rule(Option("1")) should be(Ior.right(Some(1)))
    rule(None) should be(Ior.right(None))
    rule(Option("a")) should be(Ior.left(Message.errors("Must be a whole number")))
  }

  "required" in {
    val rule = rules.required(rules.parseInt)
    rule(Option("1")) should be(Ior.right(1))
    rule(None) should be(Ior.left(Message.errors("Value is required")))
    rule(Option("a")) should be(Ior.left(Message.errors("Must be a whole number")))
  }

  "sequence" in {
    val rule = rules.sequence(rules.parseInt)
    rule(List("3", "2", "1")) should be(Ior.right(List(3, 2, 1)))
    rule(List("a", "b", "c")) should be(Ior.left(Message.errors(0 -> "Must be a whole number", 1 -> "Must be a whole number", 2 -> "Must be a whole number")))
    rule(List("a", "2", "c")) should be(Ior.left(Message.errors(0 -> "Must be a whole number", 2 -> "Must be a whole number")))
  }

  "lookup" - {
    "string keys" in {
      val rule = rules.lookup[String, String]("foo")
      rule(Map.empty) should be(Ior.left(Message.errors("foo" -> "Value not found")))
      rule(Map("foo" -> "bar")) should be(Ior.right("bar"))
      rule(Map("baz" -> "bar")) should be(Ior.left(Message.errors("foo" -> "Value not found")))
    }

    "int keys" in {
      val rule = rules.lookup[Int, String](123)
      rule(Map.empty) should be(Ior.left(Message.errors(123 -> "Value not found")))
      rule(Map(123 -> "bar")) should be(Ior.right("bar"))
      rule(Map(456 -> "bar")) should be(Ior.left(Message.errors(123 -> "Value not found")))
    }
  }
}
