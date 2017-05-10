package checklist

import cats.data._
import org.scalatest._

class CheckedSpec extends WordSpec with Matchers {
  import syntax._

  val valueOnly   = Ior.right[Messages, Int](42)
  val errorAnd    = Ior.both[Messages, Int](error("Fail"), 42)
  val warningAnd  = Ior.both[Messages, Int](warning("Warn"), 42)
  val bothAnd     = Ior.both[Messages, Int](error("Fail") ++ warning("Warn").toList, 42)
  val errorOnly   = Ior.left[Messages, Int](error("Fail"))
  val warningOnly = Ior.left[Messages, Int](warning("Warn"))
  val bothOnly    = Ior.left[Messages, Int](error("Fail") ++ warning("Warn").toList)

  "checked.isValid" should {
    "only return true for a right" in {
      valueOnly.isValid should be(true)
      errorAnd.isValid should be(false)
      warningAnd.isValid should be(false)
      bothAnd.isValid should be(false)
      errorOnly.isValid should be(false)
      warningOnly.isValid should be(false)
      bothOnly.isValid should be(false)
    }
  }

  "checked.hasErrors" should {
    "only return true for a right" in {
      valueOnly.hasErrors should be(false)
      errorAnd.hasErrors should be(true)
      warningAnd.hasErrors should be(false)
      bothAnd.hasErrors should be(true)
      errorOnly.hasErrors should be(true)
      warningOnly.hasErrors should be(false)
      bothOnly.hasErrors should be(true)
    }
  }

  "checked.hasNoErrors" should {
    "only return true for a right" in {
      valueOnly.hasNoErrors should be(true)
      errorAnd.hasNoErrors should be(false)
      warningAnd.hasNoErrors should be(true)
      bothAnd.hasNoErrors should be(false)
      errorOnly.hasNoErrors should be(false)
      warningOnly.hasNoErrors should be(true)
      bothOnly.hasNoErrors should be(false)
    }
  }
  "checked.hasWarnings" should {
    "only return true for a right" in {
      valueOnly.hasWarnings should be(false)
      errorAnd.hasWarnings should be(false)
      warningAnd.hasWarnings should be(true)
      bothAnd.hasWarnings should be(true)
      errorOnly.hasWarnings should be(false)
      warningOnly.hasWarnings should be(true)
      bothOnly.hasWarnings should be(true)
    }
  }

  "checked.hasNoWarnings" should {
    "only return true for a right" in {
      valueOnly.hasNoWarnings should be(true)
      errorAnd.hasNoWarnings should be(true)
      warningAnd.hasNoWarnings should be(false)
      bothAnd.hasNoWarnings should be(false)
      errorOnly.hasNoWarnings should be(true)
      warningOnly.hasNoWarnings should be(false)
      bothOnly.hasNoWarnings should be(false)
    }
  }
}
