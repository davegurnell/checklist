package checklist

import cats.data.{Ior, NonEmptyVector}

package object std {
  type Messages = NonEmptyVector[Message]

  type IorMessages[A] = Ior[Messages, A]
}
