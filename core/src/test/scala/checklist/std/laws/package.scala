package checklist
package std

import cats.Eq
import cats.data.{Ior, NonEmptyVector}
import cats.implicits._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import checklist.std.ior.rules
import checklist.std.ior.syntax._
import org.scalacheck.Arbitrary

package object laws extends CatsInstances with ScalacheckInstances

trait CatsInstances {
  implicit def ruleEq[A: Arbitrary, B: Eq]: Eq[Rule[IorMessages, A, B]] =
    catsLawsEqForFn1[A, IorMessages[B]].contramap[Rule[IorMessages, A, B]](rule => rule.apply _)
}

trait ScalacheckInstances {
  implicit val arbPath: Arbitrary[Path] =
    Arbitrary {
      for {
        nodes <- Arbitrary.arbitrary[NonEmptyVector[Either[Int, String]]]
      } yield nodes.foldMap[Path](_.fold(PIndex(_), PField(_)))
    }

  implicit val arbMessage: Arbitrary[Message] =
    Arbitrary {
      for {
        msg  <- Arbitrary.arbitrary[String]
        path <- Arbitrary.arbitrary[Path]
        soft <- Arbitrary.arbitrary[Boolean]
      } yield if(soft) WarningMessage(msg, path) else ErrorMessage(msg, path)
    }

  // This isn't implicit because it's bad and should be used selectively:
  def arbInOut[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] =
    Arbitrary(Arbitrary.arbitrary[A].map(a => (_: A) => a))

  implicit val arbMessageFunc: Arbitrary[Message => Message] =
    arbInOut[Message]

  implicit val arbPathFunc: Arbitrary[Path => Path] =
    arbInOut[Path]
}
