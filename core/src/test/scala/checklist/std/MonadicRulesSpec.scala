package checklist
package std

import cats.data.Ior
import cats.instances.list._
import checklist.std.ior._
import org.scalatest._

class MonadicRulesSpec extends FreeSpec with Matchers {
  "sequence" in {
    val rule = rules.sequence(rules.parseInt)
    rule(List("3", "2", "1")) should be(Ior.right(List(3, 2, 1)))
    rule(List("a", "b", "c")) should be(Ior.left(Message.errors(0 -> "Must be a whole number", 1 -> "Must be a whole number", 2 -> "Must be a whole number")))
    rule(List("a", "2", "c")) should be(Ior.left(Message.errors(0 -> "Must be a whole number", 2 -> "Must be a whole number")))
  }

  "dictionary" in {
    val rule = rules.dictionary[String, String, Int](rules.parseInt)
    rule(Map("foo" -> "3", "bar" -> "2", "baz" -> "1")) should be(Ior.right(Map("foo" -> 3, "bar" -> 2, "baz" -> 1)))
    rule(Map("foo" -> "a", "bar" -> "b", "baz" -> "c")) should be(Ior.left(Message.errors("foo" -> "Must be a whole number", "bar" -> "Must be a whole number", "baz" -> "Must be a whole number")))
    rule(Map("foo" -> "a", "bar" -> "2", "baz" -> "c")) should be(Ior.left(Message.errors("foo" -> "Must be a whole number", "baz" -> "Must be a whole number")))
  }
}
