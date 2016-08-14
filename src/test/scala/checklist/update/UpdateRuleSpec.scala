package checklist
package update

import cats.data.Ior
import org.scalatest._
import UpdateRule._
import Message._

class PropertyUpdateRulesSpec extends FreeSpec with Matchers {
  "alwaysReplace" in {
    val rule = alwaysReplace[Int, Int]
    rule(0, 1) should be(Ior.right(1))
    rule(1, 0) should be(Ior.right(0))
  }

  "alwaysIgnore" in {
    val rule = alwaysIgnore[Int, Int]
    rule(1, 0) should be(Ior.right(1))
    rule(0, 1) should be(Ior.right(0))
  }

  "cannotChange" in {
    val rule = cannotChange[Int, Int]
    rule(0, 0) should be(Ior.right(0))
    rule(0, 1) should be(Ior.both(errors("Cannot be changed"), 0))
    rule(1, 0) should be(Ior.both(errors("Cannot be changed"), 1))
    rule(1, 1) should be(Ior.right(1))
  }

  "canChangeTo" in {
    val rule = canChangeTo[Int, Int]()(_ % 2 == 0)
    rule(1, 0) should be(Ior.right(0))
    rule(1, 1) should be(Ior.right(1))
    rule(1, 2) should be(Ior.right(2))
    rule(1, 3) should be(Ior.both(errors("Cannot be changed to that value"), 1))
  }
}
