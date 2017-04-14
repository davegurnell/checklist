package checklist

import cats.data.{Ior, NonEmptyList}
import org.scalatest._
import monocle.macros.Lenses
import scala.language.higherKinds
import Rule._
import Message._

class BaseRuleSpec extends FreeSpec with Matchers {
  "pass" in {
    val rule = Rule.pass[Int]
    rule(+1) should be(Ior.right(+1))
    rule(-1) should be(Ior.right(-1))
  }

  "fail" - {
    "errors" in {
      val rule = Rule.fail[Int](errors("fail"))
      rule(+1) should be(Ior.both(errors("fail"), +1))
      rule(-1) should be(Ior.both(errors("fail"), -1))
    }

    "warnings" in {
      val rule = Rule.fail[Int](warnings("fail"))
      rule(+1) should be(Ior.both(warnings("fail"), +1))
      rule(-1) should be(Ior.both(warnings("fail"), -1))
    }
  }
}

class ConverterRulesSpec extends FreeSpec with Matchers {
  "parseInt" in {
    val rule = parseInt
    rule("abc")   should be(Ior.left(errors("Must be a whole number")))
    rule("123")   should be(Ior.right(123))
    rule("123.4") should be(Ior.left(errors("Must be a whole number")))
  }

  "parseDouble" in {
    val rule = parseDouble
    rule("abc")   should be(Ior.left(errors("Must be a number")))
    rule("123")   should be(Ior.right(123.0))
    rule("123.4") should be(Ior.right(123.4))
  }

  "mapValue" - {
    "string keys" in {
      val rule = mapValue[String, String]("foo")
      rule(Map.empty) should be(Ior.left(errors("foo" -> "Value not found")))
      rule(Map("foo" -> "bar")) should be(Ior.right("bar"))
      rule(Map("baz" -> "bar")) should be(Ior.left(errors("foo" -> "Value not found")))
    }

    "int keys" in {
      val rule = mapValue[Int, String](123)
      rule(Map.empty) should be(Ior.left(errors(123 -> "Value not found")))
      rule(Map(123 -> "bar")) should be(Ior.right("bar"))
      rule(Map(456 -> "bar")) should be(Ior.left(errors(123 -> "Value not found")))
    }
  }

  "trimString" in {
    val rule = trimString
    rule("") should be(Ior.right(""))
    rule("foo") should be(Ior.right("foo"))
    rule(" foo ") should be(Ior.right("foo"))
  }
}

class PropertyRulesSpec extends FreeSpec with Matchers {
  "seq" in {
    import cats.instances.list._
    val rule = eql("ok").seq[List]
    rule(Nil) should be(Ior.right(Nil))
    rule(List("ok", "ok")) should be(Ior.right(List("ok", "ok")))
    rule(List("ko", "ok")) should be(Ior.both(errors(0 -> "Must be ok"), List("ko", "ok")))
    rule(List("ok", "ko")) should be(Ior.both(errors(1 -> "Must be ok"), List("ok", "ko")))
    rule(List("ko", "ko")) should be(Ior.both(errors(0 -> "Must be ok", 1 -> "Must be ok"), List("ko", "ko")))
  }

  "opt" in {
    val rule = eql("ok").opt
    rule(None)       should be(Ior.right(None))
    rule(Some("ok")) should be(Ior.right(Some("ok")))
    rule(Some("no")) should be(Ior.both(errors("Must be ok"), Some("no")))
  }

  "req" in {
    val rule = eql(0).req
    rule(None)    should be(Ior.left(errors("Value is required")))
    rule(Some(0)) should be(Ior.right(0))
    rule(Some(1)) should be(Ior.both(errors("Must be 0"), 1))
  }

  "eql" in {
    val rule = eql(0)
    rule(0)  should be(Ior.right(0))
    rule(+1) should be(Ior.both(errors("Must be 0"), +1))
    rule(-1) should be(Ior.both(errors("Must be 0"), -1))
  }

  "neq" in {
    val rule = neq(0)
    rule(0)  should be(Ior.both(errors("Must not be 0"), 0))
    rule(+1) should be(Ior.right(+1))
    rule(-1) should be(Ior.right(-1))
  }

  "lt" in {
    val rule = lt(0)
    rule(0)  should be(Ior.both(errors("Must be less than 0"), 0))
    rule(1)  should be(Ior.both(errors("Must be less than 0"), 1))
    rule(-1) should be(Ior.right(-1))
  }

  "lte" in {
    val rule = lte(0)
    rule(0)  should be(Ior.right(0))
    rule(1)  should be(Ior.both(errors("Must be less than or equal to 0"), 1))
    rule(-1) should be(Ior.right(-1))
  }

  "gt" in {
    val rule = gt(0)
    rule(0)  should be(Ior.both(errors("Must be greater than 0"), 0))
    rule(1)  should be(Ior.right(1))
    rule(-1) should be(Ior.both(errors("Must be greater than 0"), -1))
  }

  "gte" in {
    val rule = gte(0)
    rule(0)  should be(Ior.right(0))
    rule(1)  should be(Ior.right(1))
    rule(-1) should be(Ior.both(errors("Must be greater than or equal to 0"), -1))
  }

  "nonEmpty" in {
    val rule = nonEmpty[String]
    rule("")    should be(Ior.both(errors("Must not be empty"), ""))
    rule(" ")   should be(Ior.right(" "))
    rule(" a ") should be(Ior.right(" a "))
  }

  "lengthLt"  in {
    val rule = lengthLt[String](5, errors("fail"))

    rule("")      should be(Ior.right(""))
    rule("abcd")  should be(Ior.right("abcd"))
    rule("abcde") should be(Ior.both(errors("fail"), "abcde"))
  }

  "lengthLte" in {
    val rule = lengthLte[String](5, errors("fail"))

    rule("")       should be(Ior.right(""))
    rule("abcde")  should be(Ior.right("abcde"))
    rule("abcdef") should be(Ior.both(errors("fail"), "abcdef"))
  }

  "lengthGt" in {
    val rule = lengthGt[String](1, errors("fail"))

    rule("")   should be(Ior.both(errors("fail"), ""))
    rule("a")  should be(Ior.both(errors("fail"), "a"))
    rule("ab") should be(Ior.right("ab"))
  }

  "lengthGte" in {
    val rule = lengthGte[String](2, errors("fail"))

    rule("")   should be(Ior.both(errors("fail"), ""))
    rule(" ")  should be(Ior.both(errors("fail"), " "))
    rule("a")  should be(Ior.both(errors("fail"), "a"))
    rule("ab") should be(Ior.right("ab"))
  }

  "matchesRegex" in {
    val rule = matchesRegex("^[^@]+@[^@]+$".r)
    rule("dave@example.com")  should be(Ior.right("dave@example.com"))
    rule("dave@")             should be(Ior.both(errors("Must match the pattern '^[^@]+@[^@]+$'"), "dave@"))
    rule("@example.com")      should be(Ior.both(errors("Must match the pattern '^[^@]+@[^@]+$'"), "@example.com"))
    rule("dave@@example.com") should be(Ior.both(errors("Must match the pattern '^[^@]+@[^@]+$'"), "dave@@example.com"))
  }

  "notContainedIn" in {
    val rule = notContainedIn(List(1, 2, 3))
    rule(0) should be(Ior.right(0))
    rule(1) should be(Ior.both(errors("Must not be one of the values 1, 2, 3"), 1))
    rule(2) should be(Ior.both(errors("Must not be one of the values 1, 2, 3"), 2))
    rule(3) should be(Ior.both(errors("Must not be one of the values 1, 2, 3"), 3))
    rule(4) should be(Ior.right(4))
  }

  "containedIn" in {
    val rule = containedIn(List(1, 2, 3))
    rule(0) should be(Ior.both(errors("Must be one of the values 1, 2, 3"), 0))
    rule(1) should be(Ior.right(1))
    rule(2) should be(Ior.right(2))
    rule(3) should be(Ior.right(3))
    rule(4) should be(Ior.both(errors("Must be one of the values 1, 2, 3"), 4))
  }
}

class CombinatorRulesSpec extends FreeSpec with Matchers {
  "map" in {
    val rule = Rule.gt[Int](0, errors("fail")).map(_ + "!")
    rule(+1) should be(Ior.right("1!"))
    rule( 0) should be(Ior.both(errors("fail"), "0!"))
    rule(-1) should be(Ior.both(errors("fail"), "-1!"))
  }

  "contramap" in {
    val rule = Rule.gt[Int](0, errors("fail")).contramap[Int](_ + 1)
    rule(+1) should be(Ior.right(2))
    rule( 0) should be(Ior.right(1))
    rule(-1) should be(Ior.both(errors("fail"), 0))
  }

  "flatMap" in {
    val rule = for {
      a <- gte[Int](0, warnings("Should be >= 0"))
      b <- if(a > 0) {
             lt[Int](10, errors("Must be < 10"))
           } else {
             gt[Int](-10, errors("Must be > -10"))
           }
    } yield b
    rule(  0) should be(Ior.right(0))
    rule( 10) should be(Ior.both(errors("Must be < 10"), 10))
    rule(-10) should be(Ior.both(warnings("Should be >= 0") concat errors("Must be > -10"), -10))
  }

  "andThen" in {
    val rule = parseInt andThen gt(0)
    rule("abc") should be(Ior.left(errors("Must be a whole number")))
    rule( "-1") should be(Ior.both(errors("Must be greater than 0"), -1))
    rule(  "0") should be(Ior.both(errors("Must be greater than 0"), 0))
    rule( "+1") should be(Ior.right(1))
    rule("1.2") should be(Ior.left(errors("Must be a whole number")))
  }

  "zip" in {
    val rule = parseInt zip parseDouble
    rule("abc") should be(Ior.left(errors("Must be a whole number", "Must be a number")))
    rule(  "1") should be(Ior.right((1, 1.0)))
    rule("1.2") should be(Ior.left(errors("Must be a whole number")))
  }

  "prefix" in {
    val rule = parseInt andThen gt(0) prefix "num"
    rule("abc") should be(Ior.left(errors("num" -> "Must be a whole number")))
    rule( "-1") should be(Ior.both(errors("num" -> "Must be greater than 0"), -1))
    rule(  "0") should be(Ior.both(errors("num" -> "Must be greater than 0"), 0))
    rule( "+1") should be(Ior.right(1))
    rule("1.2") should be(Ior.left(errors("num" -> "Must be a whole number")))
  }

  "composeLens" in {
    val l = monocle.Lens[(Int, Int), Int](p => p._1)(n => p => (n, p._2))
    val r = monocle.Lens[(Int, Int), Int](p => p._2)(n => p => (p._1, n))
    val rule = (lt(0) composeLens l) andThen (gt(0) composeLens r)
    rule(( 0,  0)) should be(Ior.both(errors("Must be less than 0", "Must be greater than 0"), (0, 0)))
    rule((+1, +1)) should be(Ior.both(errors("Must be less than 0"), (1, 1)))
    rule((-1, -1)) should be(Ior.both(errors("Must be greater than 0"), (-1, -1)))
    rule((+1, -1)) should be(Ior.both(errors("Must be less than 0", "Must be greater than 0"), (1, -1)))
    rule((-1, +1)) should be(Ior.right((-1, 1)))
  }

  "at" in {
    val l = monocle.Lens[(Int, Int), Int](p => p._1)(n => p => (n, p._2))
    val r = monocle.Lens[(Int, Int), Int](p => p._2)(n => p => (p._1, n))
    val rule = lt(0).at("l", l) andThen gt(0).at("r", r)
    rule(( 0,  0)) should be(Ior.both(errors("l" -> "Must be less than 0", "r" -> "Must be greater than 0"), (0, 0)))
    rule((+1, +1)) should be(Ior.both(errors("l" -> "Must be less than 0"), (1, 1)))
    rule((-1, -1)) should be(Ior.both(errors("r" -> "Must be greater than 0"), (-1, -1)))
    rule((+1, -1)) should be(Ior.both(errors("l" -> "Must be less than 0", "r" -> "Must be greater than 0"), (1, -1)))
    rule((-1, +1)) should be(Ior.right((-1, 1)))
  }
}

class CatsRuleSpec extends FreeSpec with Matchers {
  import cats._
  import cats.data._
  import cats.syntax.all._

  case class Address(house: Int, street: String)

  def getField(name: String): Rule[Map[String, String], String] =
    pure(_.get(name).map(Ior.right).getOrElse(Ior.left(errors(s"Field not found: ${name}"))))

  val parseAddress = (
    (getField("house")  andThen parseInt andThen gt(0)) |@|
    (getField("street") andThen nonEmpty)
  ) map (Address.apply)

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

class Rule1SyntaxSpec extends FreeSpec with Matchers {
  @Lenses case class Coord(x: Int, y: Int)

  "field macro" in {
    val rule = Rule.pass[Coord].field(_.x)(gt(0, errors("fail")))
    rule(Coord(1, 0)) should be(Ior.right(Coord(1, 0)))
    rule(Coord(0, 0)) should be(Ior.both(errors(("x" :: PNil) -> "fail"), Coord(0, 0)))
    rule(Coord(0, 1)) should be(Ior.both(errors(("x" :: PNil) -> "fail"), Coord(0, 1)))
    rule(Coord(1, 1)) should be(Ior.right(Coord(1, 1)))
  }

  "fieldWith macro" in {
    val rule = Rule.pass[Coord].fieldWith(_.x)(c => gte(c.y, errors("fail")))
    rule(Coord(1, 0)) should be(Ior.right(Coord(1, 0)))
    rule(Coord(0, 0)) should be(Ior.right(Coord(0, 0)))
    rule(Coord(0, 1)) should be(Ior.both(errors(("x" :: PNil) -> "fail"), Coord(0, 1)))
    rule(Coord(1, 1)) should be(Ior.right(Coord(1, 1)))
  }

  "field method" in {
    val rule = Rule.pass[Coord].field("z" :: PNil, Coord.x)(gt(0, errors("fail")))
    rule(Coord(1, 0)) should be(Ior.right(Coord(1, 0)))
    rule(Coord(0, 0)) should be(Ior.both(errors(("z" :: PNil) -> "fail"), Coord(0, 0)))
    rule(Coord(0, 1)) should be(Ior.both(errors(("z" :: PNil) -> "fail"), Coord(0, 1)))
    rule(Coord(1, 1)) should be(Ior.right(Coord(1, 1)))
  }

  "fieldWith method" in {
    val rule = Rule.pass[Coord].fieldWith("z" :: PNil, Coord.x)(c => gte(c.y, errors("fail")))
    rule(Coord(1, 0)) should be(Ior.right(Coord(1, 0)))
    rule(Coord(0, 0)) should be(Ior.right(Coord(0, 0)))
    rule(Coord(0, 1)) should be(Ior.both(errors(("z" :: PNil) -> "fail"), Coord(0, 1)))
    rule(Coord(1, 1)) should be(Ior.right(Coord(1, 1)))
  }
}
