package checklist.laws

import cats.Eq
import cats.data.{Ior, NonEmptyList}
import cats.laws.discipline.{ApplicativeTests, ProfunctorTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import checklist._
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import cats.implicits._

class RuleLawTests extends FunSuite with Discipline {

  implicit def arbRule[A: Arbitrary, B: Arbitrary](implicit arbF: Arbitrary[A => Ior[A, B]]): Arbitrary[Rule[A, B]] = Arbitrary(
    for {
      f <- arbF.arbitrary
    } yield Rule.pure(f.map {
      case Ior.Both(_, b) => Ior.both(NonEmptyList.of(ErrorMessage("both")), b)
      case Ior.Left(_) => Ior.left(NonEmptyList.of(ErrorMessage("left")))
      case Ior.Right(a) => Ior.right(a)
    })
  )

  implicit def ruleEq[A: Arbitrary, B: Eq]: Eq[Rule[A, B]] =
    catsLawsEqForFn1[A, Checked[B]].contramap[Rule[A, B]](rule => rule.apply _)


  checkAll("Rule[Int, String]", ApplicativeTests[Rule[Int, ?]].applicative[String, String, String])
  checkAll("Rule[Int, String]", ProfunctorTests[Rule].profunctor[Int, Int, Int, String, String, String])
}
