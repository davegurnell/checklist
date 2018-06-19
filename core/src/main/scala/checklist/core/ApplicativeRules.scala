package checklist
package core

import cats._
import cats.implicits._

trait ApplicativeRules[F[_]] {
  self: BaseRules[F] =>

  implicit protected def applicative: Applicative[F]

  def optional[A, B](rule: Rule[F, A, B]): Rule[F, Option[A], Option[B]] =
    pure {
      case Some(value) => rule(value).map(Some(_))
      case None        => checked.pass(None)
    }

  def required[A, B](rule: Rule[F, A, B]): Rule[F, Option[A], B] =
    required(rule, fatal("Value is required"))

  def required[A, B](rule: Rule[F, A, B], orElse: Rule[F, Option[A], B]): Rule[F, Option[A], B] =
    pure {
      case Some(value) => rule(value)
      case None        => orElse(None)
    }

  def lookup[A, B](key: A)(implicit prefix: ToPath[A]): Rule[F, Map[A, B], B] =
    lookup[A, B](key, fatal[Map[A, B], B](s"Value not found"))

  def lookup[A, B](key: A, orElse: Rule[F, Map[A, B], B])(implicit prefix: ToPath[A]): Rule[F, Map[A, B], B] =
    pure(dict => checked.prefix(dict.get(key).fold(orElse(dict))(checked.pass), key))
}
