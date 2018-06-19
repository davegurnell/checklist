import cats.data.Kleisli

package object checklist {
  type Rule[F[_], A, B] = Kleisli[F, A, B]
  type Rule1[F[_], A] = Kleisli[F, A, A]
}
