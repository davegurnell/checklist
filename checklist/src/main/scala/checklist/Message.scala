package checklist

import cats.{Eq, Order}
import cats.data.NonEmptyList

final case class Message(text: String, path: Path = PNil) {
  def prefix[P: Prefix](p: P): Message = copy(path = path prefix p)

}

object Message {
  def messages[A](head: A, tail: A *)(implicit promoter: ToMessage[A]): Messages =
    NonEmptyList.of(head, tail : _*).map(promoter.toError)

  implicit def orderChecklistMessage: Order[Message] = Order.by[Message, Path](_.path)

  implicit def eqChecklistMessage: Eq[Message] =
    Eq.fromUniversalEquals[Message]
}

