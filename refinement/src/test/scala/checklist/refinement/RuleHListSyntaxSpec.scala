package checklist.refinement

import checklist._
import checklist.Message._
import cats.data.{NonEmptyList, Ior}
import org.scalatest._
import org.scalatest.freespec._
import org.scalatest.matchers.should._

class RuleBuilderSpec extends AnyFreeSpec with Matchers with RuleHListSyntax {
  case class RawFoo(
      positive: Int,
      potentiallyEmptyList: List[String],
      untrimmed: String
  )

  "check" - {
    case class ValidatedFoo(
        positive: Int,
        nonEmptyList: NonEmptyList[String],
        trimmed: String
    )
    "with path" - {
      val rule =
        Rule
          .builder[RawFoo]
          .check("positive", _.positive)(Rule.gte(0, errors("negative")))
          .check("potentiallyEmptyList", _.potentiallyEmptyList)(
            Rule.nonEmptyList(errors("empty"))
          )
          .check("untrimmed", _.untrimmed)(Rule.trimString)
          .build[ValidatedFoo]

      "soft-failures should yield both" in {
        rule(RawFoo(-10, List("foo"), "bar")) should be(
          Ior.Both(
            NonEmptyList.of(ErrorMessage("negative", PField("positive", PNil))),
            ValidatedFoo(-10, NonEmptyList.of("foo"), "bar")
          )
        )
      }

      "transformations should occur" in {
        rule(RawFoo(1, List("foo"), "bar ")) should be(
          Ior.Right(ValidatedFoo(1, NonEmptyList.of("foo"), "bar"))
        )
      }

      "hard failures should yield left" in {
        rule(RawFoo(1, List(), "bar")) should be(
          Ior.Left(
            NonEmptyList
              .of(ErrorMessage("empty", PField("potentiallyEmptyList", PNil)))
          )
        )
      }
    }

    "without path" - {
      val rule =
        Rule
          .builder[RawFoo]
          .check(_.positive)(Rule.gte(0, errors("negative")))
          .check(_.potentiallyEmptyList)(Rule.nonEmptyList(errors("empty")))
          .check(_.untrimmed)(Rule.trimString)
          .build[ValidatedFoo]

      "soft-failures should yield both" in {
        rule(RawFoo(-10, List("foo"), "bar")) should be(
          Ior.Both(
            NonEmptyList.of(ErrorMessage("negative", PNil)),
            ValidatedFoo(-10, NonEmptyList.of("foo"), "bar")
          )
        )
      }

      "transformations should occur" in {
        rule(RawFoo(1, List("foo"), "bar ")) should be(
          Ior.Right(ValidatedFoo(1, NonEmptyList.of("foo"), "bar"))
        )
      }

      "hard failures should yield left" in {
        rule(RawFoo(1, List(), "bar")) should be(
          Ior.Left(NonEmptyList.of(ErrorMessage("empty", PNil)))
        )
      }
    }
  }

  "checkAndDrop" - {
    case class ValidatedFoo(nonEmptyList: NonEmptyList[String], trimmed: String)
    "with path" - {
      val rule =
        Rule
          .builder[RawFoo]
          .checkAndDrop("positive", _.positive)(Rule.gte(0, errors("negative")))
          .check("potentiallyEmptyList", _.potentiallyEmptyList)(
            Rule.nonEmptyList(errors("empty"))
          )
          .check("untrimmed", _.untrimmed)(Rule.trimString)
          .build[ValidatedFoo]

      "soft-failures should yield both" in {
        rule(RawFoo(-10, List("foo"), "bar")) should be(
          Ior.Both(
            NonEmptyList.of(ErrorMessage("negative", PField("positive", PNil))),
            ValidatedFoo(NonEmptyList.of("foo"), "bar")
          )
        )
      }

      "transformations should occur" in {
        rule(RawFoo(1, List("foo"), "bar ")) should be(
          Ior.Right(ValidatedFoo(NonEmptyList.of("foo"), "bar"))
        )
      }

      "hard failures should yield left" in {
        rule(RawFoo(1, List(), "bar")) should be(
          Ior.Left(
            NonEmptyList
              .of(ErrorMessage("empty", PField("potentiallyEmptyList", PNil)))
          )
        )
      }
    }

    "without path" - {
      val rule =
        Rule
          .builder[RawFoo]
          .checkAndDrop(_.positive)(Rule.gte(0, errors("negative")))
          .check(_.potentiallyEmptyList)(Rule.nonEmptyList(errors("empty")))
          .check(_.untrimmed)(Rule.trimString)
          .build[ValidatedFoo]

      "soft-failures should yield both" in {
        rule(RawFoo(-10, List("foo"), "bar")) should be(
          Ior.Both(
            NonEmptyList.of(ErrorMessage("negative", PNil)),
            ValidatedFoo(NonEmptyList.of("foo"), "bar")
          )
        )
      }

      "transformations should occur" in {
        rule(RawFoo(1, List("foo"), "bar ")) should be(
          Ior.Right(ValidatedFoo(NonEmptyList.of("foo"), "bar"))
        )
      }

      "hard failures should yield left" in {
        rule(RawFoo(1, List(), "bar")) should be(
          Ior.Left(NonEmptyList.of(ErrorMessage("empty", PNil)))
        )
      }
    }
  }

  "append" - {
    case class Foo(n: Int)
    val rule = Rule.builder[Unit].append(5).build[Foo]
    "appends a value" in {
      rule(()) should be(Ior.Right(Foo(5)))
    }
  }

  "pass" - {
    case class Foo(n: Int)
    case class Bar(n: Int)
    val rule = Rule.builder[Foo].pass(_.n).build[Bar]
    "passes a value with no modification/validation" in {
      rule(Foo(5)) should be(Ior.Right(Bar(5)))
    }
  }
}
