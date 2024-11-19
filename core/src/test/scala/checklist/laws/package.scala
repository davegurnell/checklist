package checklist

import cats.Eq
import cats.data.{Ior, NonEmptyList}
import cats.implicits.*
import cats.laws.discipline.ExhaustiveCheck
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import org.scalacheck.Arbitrary

package object laws extends CatsInstances with ScalacheckInstances

trait ScalacheckInstances {
  implicit def arbRule[A: Arbitrary, B: Arbitrary](implicit arbF: Arbitrary[A => Ior[A, B]]): Arbitrary[Rule[A, B]] = Arbitrary(
    for {
      f        <- arbF.arbitrary
      messages <- Arbitrary.arbitrary[Messages]
    } yield Rule.pure(
      f(_).fold(
        _ => Ior.left(messages),
        Ior.right,
        (_, b) => Ior.both(messages, b)
      )
    )
  )

  implicit val arbPath: Arbitrary[Path] = Arbitrary(
    for {
      nodes <- Arbitrary.arbitrary[NonEmptyList[Either[Int, String]]]
    } yield nodes.foldMap[Path](_.fold(PIndex(_), PField(_)))
  )

  implicit val arbMessage: Arbitrary[Message] = Arbitrary(
    for {
      arbMessage <- Arbitrary.arbitrary[String]
      arbPath    <- Arbitrary.arbitrary[Path]
      soft       <- Arbitrary.arbitrary[Boolean]
    } yield if (soft) WarningMessage(arbMessage, arbPath) else ErrorMessage(arbMessage, arbPath)
  )

  // not implicit because this is bad and should be used selectively.
  def arbInOut[A: Arbitrary]: Arbitrary[A => A] = Arbitrary(
    Arbitrary.arbitrary[A].map(a => (_: A) => a)
  )

  implicit val arbMessageF: Arbitrary[Message => Message] = arbInOut[Message]
  implicit val arbPathF:    Arbitrary[Path => Path]       = arbInOut[Path]

}

trait CatsInstances {
  implicit def ruleEq[A: Arbitrary: ExhaustiveCheck, B: Eq]: Eq[Rule[A, B]] =
    catsLawsEqForFn1Exhaustive[A, Checked[B]].contramap[Rule[A, B]](rule => rule.apply)
}
