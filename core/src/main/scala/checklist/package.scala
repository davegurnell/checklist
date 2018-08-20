import cats.Id
import cats.data.{Ior, NonEmptyList}

import scala.language.higherKinds

package object checklist extends CanLiftImplicits {
  type Messages         = NonEmptyList[Message]
  type Checked[A]       = Messages Ior A
  type Rule1[F[_], A]   = Rule[F, A, A]

  // We have to model this some other way.
  // A Check isn't just a rule that returns the same type, but it actually returns the same value. A "pure validator".
  type Check[F[_], A]   = Rule[F, A, A]
  type CheckNow[A]      = Check[Id, A]
}
