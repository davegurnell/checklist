package checklist
package core

import cats.data._

/**
 * Base rule constructors.
 */
trait BaseRules[F[_]] {
  implicit protected def checked: Checked[F]

  def apply[A](implicit rule: Rule[F, A, A]): Rule[F, A, A] =
    rule

  def pure[A, B](func: A => F[B]): Rule[F, A, B] =
    Kleisli(func)

  def pass[A]: Rule[F, A, A] =
    pure(checked.pass)

  def warn[A](message: String, strict: Boolean = false): Rule[F, A, A] =
    pure(value => if(strict) checked.warn(message) else checked.warn(message, value))

  def fail[A](message: String, strict: Boolean = false): Rule[F, A, A] =
    pure(value => if(strict) checked.fail(message) else checked.fail(message, value))

  def fatal[A, B](message: String): Rule[F, A, B] =
    pure(_ => checked.fail(message))
}
