package checklist
package std

import cats.data.Ior
import cats.instances.string._
import checklist.std.ior._
import org.scalatest._

class PropertyRulesSpec extends FreeSpec with Matchers {
  "non-strict" - {
    "eql" in {
      val rule = rules.eql(0)
      rule(0)  should be(Ior.right(0))
      rule(+1) should be(Ior.both(Message.errors("Must be 0"), +1))
      rule(-1) should be(Ior.both(Message.errors("Must be 0"), -1))
    }

    "neq" in {
      val rule = rules.neq(0)
      rule(0)  should be(Ior.both(Message.errors("Must not be 0"), 0))
      rule(+1) should be(Ior.right(+1))
      rule(-1) should be(Ior.right(-1))
    }

    "lt" in {
      val rule = rules.lt(0)
      rule(0)  should be(Ior.both(Message.errors("Must be less than 0"), 0))
      rule(1)  should be(Ior.both(Message.errors("Must be less than 0"), 1))
      rule(-1) should be(Ior.right(-1))
    }

    "lte" in {
      val rule = rules.lte(0)
      rule(0)  should be(Ior.right(0))
      rule(1)  should be(Ior.both(Message.errors("Must be less than or equal to 0"), 1))
      rule(-1) should be(Ior.right(-1))
    }

    "gt" in {
      val rule = rules.gt(0)
      rule(0)  should be(Ior.both(Message.errors("Must be greater than 0"), 0))
      rule(1)  should be(Ior.right(1))
      rule(-1) should be(Ior.both(Message.errors("Must be greater than 0"), -1))
    }

    "gte" in {
      val rule = rules.gte(0)
      rule(0)  should be(Ior.right(0))
      rule(1)  should be(Ior.right(1))
      rule(-1) should be(Ior.both(Message.errors("Must be greater than or equal to 0"), -1))
    }

    "nonEmpty" in {
      val rule = rules.nonEmpty[String]
      rule("")    should be(Ior.both(Message.errors("Must not be empty"), ""))
      rule(" ")   should be(Ior.right(" "))
      rule(" a ") should be(Ior.right(" a "))
    }

    "lengthLt"  in {
      val rule = rules.lengthLt(5, rules.fail("fail"))

      rule("")      should be(Ior.right(""))
      rule("abcd")  should be(Ior.right("abcd"))
      rule("abcde") should be(Ior.both(Message.errors("fail"), "abcde"))
    }

    "lengthLte" in {
      val rule = rules.lengthLte(5, rules.fail("fail"))

      rule("")       should be(Ior.right(""))
      rule("abcde")  should be(Ior.right("abcde"))
      rule("abcdef") should be(Ior.both(Message.errors("fail"), "abcdef"))
    }

    "lengthGt" in {
      val rule = rules.lengthGt(1, rules.fail("fail"))

      rule("")   should be(Ior.both(Message.errors("fail"), ""))
      rule("a")  should be(Ior.both(Message.errors("fail"), "a"))
      rule("ab") should be(Ior.right("ab"))
    }

    "lengthGte" in {
      val rule = rules.lengthGte(2, rules.fail("fail"))

      rule("")   should be(Ior.both(Message.errors("fail"), ""))
      rule(" ")  should be(Ior.both(Message.errors("fail"), " "))
      rule("a")  should be(Ior.both(Message.errors("fail"), "a"))
      rule("ab") should be(Ior.right("ab"))
    }

    "matchesRegex" in {
      val rule = rules.matchesRegex("^[^@]+@[^@]+$".r)
      rule("dave@example.com")  should be(Ior.right("dave@example.com"))
      rule("dave@")             should be(Ior.both(Message.errors("Must match the pattern '^[^@]+@[^@]+$'"), "dave@"))
      rule("@example.com")      should be(Ior.both(Message.errors("Must match the pattern '^[^@]+@[^@]+$'"), "@example.com"))
      rule("dave@@example.com") should be(Ior.both(Message.errors("Must match the pattern '^[^@]+@[^@]+$'"), "dave@@example.com"))
    }

    "notContainedIn" in {
      val rule = rules.notContainedIn(List(1, 2, 3))
      rule(0) should be(Ior.right(0))
      rule(1) should be(Ior.both(Message.errors("Must not be one of the values 1, 2, 3"), 1))
      rule(2) should be(Ior.both(Message.errors("Must not be one of the values 1, 2, 3"), 2))
      rule(3) should be(Ior.both(Message.errors("Must not be one of the values 1, 2, 3"), 3))
      rule(4) should be(Ior.right(4))
    }

    "containedIn" in {
      val rule = rules.containedIn(List(1, 2, 3))
      rule(0) should be(Ior.both(Message.errors("Must be one of the values 1, 2, 3"), 0))
      rule(1) should be(Ior.right(1))
      rule(2) should be(Ior.right(2))
      rule(3) should be(Ior.right(3))
      rule(4) should be(Ior.both(Message.errors("Must be one of the values 1, 2, 3"), 4))
    }
  }

//  "strict" - {
//    "eqlStrict" in {
//      val rule = rules.eqlStrict(0)
//      rule(0)  should be(Ior.right(0))
//      rule(+1) should be(Ior.left(Message.errors("Must be 0")))
//      rule(-1) should be(Ior.left(Message.errors("Must be 0")))
//    }
//
//    "neqStrict" in {
//      val rule = rules.neqStrict(0)
//      rule(0)  should be(Ior.left(Message.errors("Must not be 0")))
//      rule(+1) should be(Ior.right(+1))
//      rule(-1) should be(Ior.right(-1))
//    }
//
//    "ltStrict" in {
//      val rule = rules.ltStrict(0)
//      rule(0)  should be(Ior.left(Message.errors("Must be less than 0")))
//      rule(1)  should be(Ior.left(Message.errors("Must be less than 0")))
//      rule(-1) should be(Ior.right(-1))
//    }
//
//    "lteStrict" in {
//      val rule = rules.lteStrict(0)
//      rule(0)  should be(Ior.right(0))
//      rule(1)  should be(Ior.left(Message.errors("Must be less than or equal to 0")))
//      rule(-1) should be(Ior.right(-1))
//    }
//
//    "gtStrict" in {
//      val rule = rules.gtStrict(0)
//      rule(0)  should be(Ior.left(Message.errors("Must be greater than 0")))
//      rule(1)  should be(Ior.right(1))
//      rule(-1) should be(Ior.left(Message.errors("Must be greater than 0")))
//    }
//
//    "gteStrict" in {
//      val rule = rules.gteStrict(0)
//      rule(0)  should be(Ior.right(0))
//      rule(1)  should be(Ior.right(1))
//      rule(-1) should be(Ior.left(Message.errors("Must be greater than or equal to 0")))
//    }
//
//    "nonEmptyStrict" in {
//      val rule = rules.nonEmptyStrict[String]
//      rule("")    should be(Ior.left(Message.errors("Must not be empty")))
//      rule(" ")   should be(Ior.right(" "))
//      rule(" a ") should be(Ior.right(" a "))
//    }
//
//    "lengthLt"  in {
//      val rule = rules.lengthLtStrict(5, Message.errors("fail"))
//
//      rule("")      should be(Ior.right(""))
//      rule("abcd")  should be(Ior.right("abcd"))
//      rule("abcde") should be(Ior.left(Message.errors("fail")))
//    }
//
//    "lengthLteStrict" in {
//      val rule = rules.lengthLteStrict[String](5, Message.errors("fail"))
//
//      rule("")       should be(Ior.right(""))
//      rule("abcde")  should be(Ior.right("abcde"))
//      rule("abcdef") should be(Ior.left(Message.errors("fail")))
//    }
//
//    "lengthGtStrict" in {
//      val rule = rules.lengthGtStrict[String](1, Message.errors("fail"))
//
//      rule("")   should be(Ior.left(Message.errors("fail")))
//      rule("a")  should be(Ior.left(Message.errors("fail")))
//      rule("ab") should be(Ior.right("ab"))
//    }
//
//    "lengthGteStrict" in {
//      val rule = rules.lengthGteStrict[String](2, Message.errors("fail"))
//
//      rule("")   should be(Ior.left(Message.errors("fail")))
//      rule(" ")  should be(Ior.left(Message.errors("fail")))
//      rule("a")  should be(Ior.left(Message.errors("fail")))
//      rule("ab") should be(Ior.right("ab"))
//    }
//
//    "matchesRegexStrict" in {
//      val rule = rules.matchesRegexStrict("^[^@]+@[^@]+$".r)
//      rule("dave@example.com")  should be(Ior.right("dave@example.com"))
//      rule("dave@")             should be(Ior.left(Message.errors("Must match the pattern '^[^@]+@[^@]+$'")))
//      rule("@example.com")      should be(Ior.left(Message.errors("Must match the pattern '^[^@]+@[^@]+$'")))
//      rule("dave@@example.com") should be(Ior.left(Message.errors("Must match the pattern '^[^@]+@[^@]+$'")))
//    }
//
//    "notContainedInStrict" in {
//      val rule = rules.notContainedInStrict(List(1, 2, 3))
//      rule(0) should be(Ior.right(0))
//      rule(1) should be(Ior.left(Message.errors("Must not be one of the values 1, 2, 3")))
//      rule(2) should be(Ior.left(Message.errors("Must not be one of the values 1, 2, 3")))
//      rule(3) should be(Ior.left(Message.errors("Must not be one of the values 1, 2, 3")))
//      rule(4) should be(Ior.right(4))
//    }
//
//    "containedInStrict" in {
//      val rule = rules.containedInStrict(List(1, 2, 3))
//      rule(0) should be(Ior.left(Message.errors("Must be one of the values 1, 2, 3")))
//      rule(1) should be(Ior.right(1))
//      rule(2) should be(Ior.right(2))
//      rule(3) should be(Ior.right(3))
//      rule(4) should be(Ior.left(Message.errors("Must be one of the values 1, 2, 3")))
//    }
//  }
}
