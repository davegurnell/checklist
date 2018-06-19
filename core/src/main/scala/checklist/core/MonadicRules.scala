package checklist
package core

import cats.{Monad, Parallel, Traverse}
import cats.implicits._

trait MonadicRules[F[_], G[_]] {
  self: BaseRules[F] =>

  implicit protected def monad: Monad[F]
  implicit protected def parallel: Parallel[F, G]

  def sequence[S[_] <: Traversable[_], A, B](rule: Rule[F, A, B])(implicit traverse: Traverse[S]): Rule[F, S[A], S[B]] =
    pure { in: S[A] =>
      in.zipWithIndex.parTraverse {
        case (value, index) =>
          checked.prefix(rule(value), index)
      }
    }

  def dictionary[A, B, C](rule: Rule[F, B, C])(implicit prefix: ToPath[A]): Rule[F, Map[A, B], Map[A, C]] =
    pure { in: Map[A, B] =>
      in.toList.parTraverse {
        case (key, value) =>
          checked.prefix(rule(value), key).map(key -> _)
      }
    }.map(_.toMap)
}
