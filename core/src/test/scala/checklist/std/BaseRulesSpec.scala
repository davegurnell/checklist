package checklist
package std

import cats.data.Ior
import checklist.std.ior._
import org.scalatest._

class BaseRulesSpec extends FreeSpec with Matchers {
  "pass" in {
    val rule = rules.pass[Int]
    rule(+1) should be(Ior.right(+1))
    rule(-1) should be(Ior.right(-1))
  }

  "fail" in {
    val rule = rules.fail[Int]("fail")
    rule(+1) should be(Ior.both(Message.errors("fail"), +1))
    rule(-1) should be(Ior.both(Message.errors("fail"), -1))
  }

  "warn" in {
    val rule = rules.warn[Int]("warn")
    rule(+1) should be(Ior.both(Message.warnings("warn"), +1))
    rule(-1) should be(Ior.both(Message.warnings("warn"), -1))
  }
}
