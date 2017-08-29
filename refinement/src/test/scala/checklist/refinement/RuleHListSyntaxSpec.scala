package checklist.refinement

import checklist._
import checklist.Message._
import org.scalatest._
import cats.data.{NonEmptyList, Ior}

class RuleBuilderSpec extends FreeSpec with Matchers with RuleHListSyntax {
  case class RawFoo(positive: Int, potentiallyEmptyList: List[String], untrimmed: String)
  case class ValidatedFoo(positive: Int, nonEmptyList: NonEmptyList[String], trimmed: String)


  "check (with path)" - {
    val rule =
      Rule.builder[RawFoo]
        .check("positive", _.positive)(Rule.gte(0, errors("negative")))
        .check("potentiallyEmptyList", _.potentiallyEmptyList)(Rule.nonEmptyList(errors("empty")))
        .check("untrimmed", _.untrimmed)(Rule.trimString)
        .build[ValidatedFoo]

    "soft-failures should yield both" in {
      rule(RawFoo(-10, List("foo"), "bar")) should be(Ior.Both(NonEmptyList.of(ErrorMessage("negative", PField("positive", PNil))), ValidatedFoo(-10, NonEmptyList.of("foo"), "bar")))
    }

    "transformations should occur" in {
      rule(RawFoo(1, List("foo"), "bar ")) should be(Ior.Right(ValidatedFoo(1, NonEmptyList.of("foo"), "bar")))
    }

    "hard failures should yield left" in {
      rule(RawFoo(1, List(), "bar")) should be(Ior.Left(NonEmptyList.of(ErrorMessage("empty", PField("potentiallyEmptyList", PNil)))))
    }
  }

  "check (without path)" - {
    val rule =
      Rule.builder[RawFoo]
        .check(_.positive)(Rule.gte(0, errors("negative")))
        .check(_.potentiallyEmptyList)(Rule.nonEmptyList(errors("empty")))
        .check(_.untrimmed)(Rule.trimString)
        .build[ValidatedFoo]

    "soft-failures should yield both" in {
      rule(RawFoo(-10, List("foo"), "bar")) should be(Ior.Both(NonEmptyList.of(ErrorMessage("negative", PNil)), ValidatedFoo(-10, NonEmptyList.of("foo"), "bar")))
    }

    "transformations should occur" in {
      rule(RawFoo(1, List("foo"), "bar ")) should be(Ior.Right(ValidatedFoo(1, NonEmptyList.of("foo"), "bar")))
    }

    "hard failures should yield left" in {
      rule(RawFoo(1, List(), "bar")) should be(Ior.Left(NonEmptyList.of(ErrorMessage("empty", PNil))))
    }
  }
}
