package checklist

import cats.data.NonEmptyList

sealed abstract class Message(val isError: Boolean, val isWarning: Boolean) {
  def text: String
  def path: Path

  def prefix[A: PathPrefix](prefix: A): Message = this match {
    case result @ ErrorMessage(text, path)   => result.copy(path = prefix :: path)
    case result @ WarningMessage(text, path) => result.copy(path = prefix :: path)
  }
}

final case class ErrorMessage(text: String, path: Path = PNil) extends Message(true, false)

final case class WarningMessage(text: String, path: Path = PNil) extends Message(false, true)

object Message extends MessageConstructors

trait MessageConstructors {
  def errors[A](head: A, tail: A *)(implicit promoter: ToMessage[A]): NonEmptyList[Message] =
    NonEmptyList.of(head, tail : _*).map(promoter.toError)

  def warnings[A](head: A, tail: A *)(implicit promoter: ToMessage[A]): NonEmptyList[Message] =
    NonEmptyList.of(head, tail : _*).map(promoter.toWarning)
}
