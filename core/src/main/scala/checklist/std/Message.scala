package checklist
package std

import cats.{Eq, Order}
import cats.data.NonEmptyVector

sealed abstract class Message(val isError: Boolean, val isWarning: Boolean) {
  def text: String
  def path: Path

  def prefix[A: ToPath](prefix: A): Message = this match {
    case result @ ErrorMessage(_, path)   => result.copy(path = prefix :: path)
    case result @ WarningMessage(_, path) => result.copy(path = prefix :: path)
  }
}

final case class ErrorMessage(text: String, path: Path = PNil) extends Message(true, false)

final case class WarningMessage(text: String, path: Path = PNil) extends Message(false, true)

object Message extends MessageConstructors with MessageInstances

trait MessageConstructors {
  def errors[A](head: A, tail: A *)(implicit promoter: ToMessage[A]): NonEmptyVector[Message] =
    NonEmptyVector.of(head, tail : _*).map(promoter.toError)

  def warnings[A](head: A, tail: A *)(implicit promoter: ToMessage[A]): NonEmptyVector[Message] =
    NonEmptyVector.of(head, tail : _*).map(promoter.toWarning)
}

trait MessageInstances {
  implicit def orderChecklistMessage: Order[Message] =
    Order.by[Message, Path](_.path)

  implicit def eqChecklistMessage: Eq[Message] =
    Eq.fromUniversalEquals[Message]
}
