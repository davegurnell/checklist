package checklist

import cats.Eq
import cats.data.{Ior, NonEmptyList}
import cats.implicits._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.Arbitrary

package object laws extends CatsInstances with ScalacheckInstances

trait ScalacheckInstances {
  implicit def arbRule[A: Arbitrary, B: Arbitrary](implicit arbF: Arbitrary[A => Ior[A, B]]): Arbitrary[Rule[A, B]] = Arbitrary(
    for {
      f <- arbF.arbitrary
    } yield Rule.pure(f.map {
      case Ior.Both(_, b) => Ior.both(NonEmptyList.of(ErrorMessage("both")), b)
      case Ior.Left(_) => Ior.left(NonEmptyList.of(ErrorMessage("left")))
      case Ior.Right(a) => Ior.right(a)
    })
  )

  implicit val arbPath: Arbitrary[Path] = Arbitrary(
    for {
      nodes <- Arbitrary.arbitrary[NonEmptyList[Either[Int, String]]]
    } yield nodes.foldMap[Path](_.fold(PIndex(_), PField(_)))
  )

  implicit val arbMessage: Arbitrary[Message] = Arbitrary(
    for {
      arbMessage <- Arbitrary.arbitrary[String]
      arbPath <- Arbitrary.arbitrary[Path]
      soft <- Arbitrary.arbitrary[Boolean]
      f = if (soft) WarningMessage.apply _ else ErrorMessage.apply _
    } yield f(arbMessage, arbPath)
  )

  // not implicit because this is bad and should be used selectively.
  def arbInOut[A: Arbitrary]: Arbitrary[A => A] = Arbitrary(
    Arbitrary.arbitrary[A].map(a => (_: A) => a)
  )

  implicit val arbMessageF: Arbitrary[Message => Message] = arbInOut[Message]
  implicit val arbPathF: Arbitrary[Path => Path] = arbInOut[Path]

}

trait CatsInstances {
  implicit def ruleEq[A: Arbitrary, B: Eq]: Eq[Rule[A, B]] =
    catsLawsEqForFn1[A, Checked[B]].contramap[Rule[A, B]](rule => rule.apply _)
}
