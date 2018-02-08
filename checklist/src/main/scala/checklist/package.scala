import cats.data.NonEmptyList

package object checklist {
  type Messages = NonEmptyList[Message]
  type Rule1[A]   = Rule[A, A]

  type Checked[+A] = Trior[Messages, Messages, A]
}
