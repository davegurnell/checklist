import cats.data.{Ior, NonEmptyList}

package object checklist {
  type Messages   = NonEmptyList[Message]
  type Checked[A] = Messages Ior A
  type Rule1[A]   = Rule[A, A]
}
