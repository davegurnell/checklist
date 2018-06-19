package checklist
package std

import cats.data.Ior
import cats.instances.string._
import cats.instances.list._
import cats.syntax.all._
import checklist.std.Message._
import checklist.std.ior._
import checklist.std.ior.syntax._
import monocle.macros.Lenses
import org.scalatest._

class RuleSyntaxSpec extends FreeSpec with Matchers {
  @Lenses case class Coord(x: Int, y: Int)

  case class Bar(baz: Int)
  case class Foo(bar: Bar)

  "RuleOps" - {
    "prefix" in {
      val rule = rules.parseInt andThen rules.gt(0) prefix "num"
      rule("abc") should be(Ior.left(errors("num" -> "Must be a whole number")))
      rule( "-1") should be(Ior.both(errors("num" -> "Must be greater than 0"), -1))
      rule(  "0") should be(Ior.both(errors("num" -> "Must be greater than 0"), 0))
      rule( "+1") should be(Ior.right(1))
      rule("1.2") should be(Ior.left(errors("num" -> "Must be a whole number")))
    }

    "composeLens" in {
      val l = monocle.Lens[(Int, Int), Int](p => p._1)(n => p => (n, p._2))
      val r = monocle.Lens[(Int, Int), Int](p => p._2)(n => p => (p._1, n))
      val rule = (rules.lt(0) composeLens l) andThen (rules.gt(0) composeLens r)
      rule(( 0,  0)) should be(Ior.both(errors("Must be less than 0", "Must be greater than 0"), (0, 0)))
      rule((+1, +1)) should be(Ior.both(errors("Must be less than 0"), (1, 1)))
      rule((-1, -1)) should be(Ior.both(errors("Must be greater than 0"), (-1, -1)))
      rule((+1, -1)) should be(Ior.both(errors("Must be less than 0", "Must be greater than 0"), (1, -1)))
      rule((-1, +1)) should be(Ior.right((-1, 1)))
    }

    "at" in {
      val l = monocle.Lens[(Int, Int), Int](p => p._1)(n => p => (n, p._2))
      val r = monocle.Lens[(Int, Int), Int](p => p._2)(n => p => (p._1, n))
      val rule = rules.lt(0).at("l", l) andThen rules.gt(0).at("r", r)
      rule(( 0,  0)) should be(Ior.both(errors("l" -> "Must be less than 0", "r" -> "Must be greater than 0"), (0, 0)))
      rule((+1, +1)) should be(Ior.both(errors("l" -> "Must be less than 0"), (1, 1)))
      rule((-1, -1)) should be(Ior.both(errors("r" -> "Must be greater than 0"), (-1, -1)))
      rule((+1, -1)) should be(Ior.both(errors("l" -> "Must be less than 0", "r" -> "Must be greater than 0"), (1, -1)))
      rule((-1, +1)) should be(Ior.right((-1, 1)))
    }
  }

  "Rule1Ops" - {
    "field macro" in {
      val rule = rules.pass[Coord]
        .field(_.x)(rules.gt(0, rules.fail("fail")))
      rule(Coord(1, 0)) should be(Ior.right(Coord(1, 0)))
      rule(Coord(0, 0)) should be(Ior.both(errors(("x" :: PNil) -> "fail"), Coord(0, 0)))
      rule(Coord(0, 1)) should be(Ior.both(errors(("x" :: PNil) -> "fail"), Coord(0, 1)))
      rule(Coord(1, 1)) should be(Ior.right(Coord(1, 1)))
    }

    "field macro with multi-level accessor" in {
      val rule = rules.pass[Foo]
        .field(_.bar.baz)(rules.gt(0, rules.fail("fail")))
      rule(Foo(Bar(1))) should be(Ior.right(Foo(Bar(1))))
      rule(Foo(Bar(0))) should be(Ior.both(errors(("bar" :: "baz" :: PNil) -> "fail"), Foo(Bar(0))))
    }

    "fieldWith macro" in {
      val rule = rules.pass[Coord]
        .fieldWith(_.x)(c => rules.gte(c.y, rules.fail("fail")))
      rule(Coord(1, 0)) should be(Ior.right(Coord(1, 0)))
      rule(Coord(0, 0)) should be(Ior.right(Coord(0, 0)))
      rule(Coord(0, 1)) should be(Ior.both(errors(("x" :: PNil) -> "fail"), Coord(0, 1)))
      rule(Coord(1, 1)) should be(Ior.right(Coord(1, 1)))
    }

    "field method" in {
      val rule = rules.pass[Coord]
        .field("z" :: PNil, Coord.x)(rules.gt(0, rules.fail("fail")))
      rule(Coord(1, 0)) should be(Ior.right(Coord(1, 0)))
      rule(Coord(0, 0)) should be(Ior.both(errors(("z" :: PNil) -> "fail"), Coord(0, 0)))
      rule(Coord(0, 1)) should be(Ior.both(errors(("z" :: PNil) -> "fail"), Coord(0, 1)))
      rule(Coord(1, 1)) should be(Ior.right(Coord(1, 1)))
    }

    "fieldWith method" in {
      val rule = rules.pass[Coord]
        .fieldWith("z" :: PNil, Coord.x)(c => rules.gte(c.y, rules.fail("fail")))
      rule(Coord(1, 0)) should be(Ior.right(Coord(1, 0)))
      rule(Coord(0, 0)) should be(Ior.right(Coord(0, 0)))
      rule(Coord(0, 1)) should be(Ior.both(errors(("z" :: PNil) -> "fail"), Coord(0, 1)))
      rule(Coord(1, 1)) should be(Ior.right(Coord(1, 1)))
    }
  }
}
