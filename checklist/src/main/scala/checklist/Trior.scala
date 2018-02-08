package checklist

import cats.implicits._
import cats.{Semigroup, Monoid, Monad}
import scala.util.{Left => SLeft, Right => SRight}

/**
 * Models data with 4 possibilities.
 *
 * The middle type will not be produced independently. It will always be paired
 * with a [[Left]] or [[Right]].
 *
 * The [[Left]] and [[Right]] sides can exist independently.
 *
 * Possibilities:
 * # L
 * # (L, M)
 * # (M, R)
 * # R
 */
sealed trait Trior[+L, +M, +R] {
  def widen[LL >: L, MM >: M, RR >: R]: Trior[LL, MM, RR] = this
  def widenLeft[LL >: L]: Trior[LL, M, R] = this
  def widenMiddle[MM >: M]: Trior[L, MM, R] = this
  def widenRight[RR >: R]: Trior[L, M, RR] = this

  def fold[A](left: L => A, leftMid: (L, M) => A, rightMid: (M, R) => A, right: R => A): A =
    this match {
      case Left(l) => left(l)
      case MidLeft(e, w) => leftMid(e, w)
      case MidRight(w, r) => rightMid(w, r)
      case Right(r) => right(r)
    }

  def recover(func: (L, Option[M]) => R): Trior[L, M, R] = recoverWith((e, w) => Right(func(e, w)))

  def recoverWith(func: (L, Option[M]) => Trior[L, M, R]): Trior[L, M, R] =
    this match {
      case Left(l) => func(l, None)
      case MidLeft(l, m) => func(l, Some(m))
      case other => other
    }

  def combine(other: Trior[L, M, R])(implicit lSemi: Semigroup[L], mSemi: Semigroup[M], rSemi: Semigroup[R]): Trior[L, M, R] = combineWith(other)(rSemi.combine)

  def combineWith[S, T](other: Trior[L, M, S])(f: (R, S) => T)(implicit lSemi: Semigroup[L], mSemi: Semigroup[M]): Trior[L, M, T] =
    this match {
      case l @ Left(left) =>
        other.fold(
          otherLeft => Left(left |+| otherLeft),
          (otherLeft, otherMiddle) => MidLeft(left |+| otherLeft, otherMiddle),
          (otherMiddle, _) => MidLeft(left, otherMiddle),
          _ => l
        )
      case l @ MidLeft(left, middle) =>
        other.fold(
          otherLeft => MidLeft(left |+| otherLeft, middle),
          (otherLeft, otherMiddle) => MidLeft(left |+| otherLeft, middle |+| otherMiddle),
          (otherMiddle, _) => MidLeft(left, middle |+| otherMiddle),
          _ => l
        )
      case MidRight(middle, right) =>
        other.fold(
          left => MidLeft(left, middle),
          (otherLeft, otherMiddle) => MidLeft(otherLeft, middle |+| otherMiddle),
          (otherMiddle, otherRight) => MidRight(middle |+| otherMiddle, f(right, otherRight)),
          otherRight => MidRight(middle, f(right, otherRight))
        )
      case Right(right) =>
        other.fold(
          Left(_),
          MidLeft(_, _),
          (middle, otherRight) => MidRight(middle, f(right, otherRight)),
          otherRight => Right(f(right, otherRight))
        )
    }

  def map[S](f: R => S): Trior[L, M, S] =
    fold(
      Left(_),
      MidLeft(_, _),
      (w, r) => MidRight(w, f(r)),
      r => Right(f(r))
    )

  def mapMiddle[X](f: M => X): Trior[L, X, R] =
    fold(
      Left(_),
      (e, w) => MidLeft(e, f(w)),
      (w, r) => MidRight(f(w), r),
      Right(_)
    )

  def mapLeft[F](f: L => F): Trior[F, M, R] =
    fold(
      l => Left(f(l)),
      (e, w) => MidLeft(f(e), w),
      MidRight(_, _),
      Right(_)
    )

  def rightValue: Option[R] =  fold(_ => None, (_, _) => None,    (_, r) => Some(r), Some(_))
  def middleValue: Option[M] = fold(_ => None, (_, w) => Some(w), (w, _) => Some(w), _ => None)
  def leftValue: Option[L] =   fold(Some(_)  , (l, _) => Some(l), (_, _) => None   , _ => None)

  def eithers: Either[Either[L, (L, M)], Either[(M, R), R]] =
    fold(
      l => SLeft(SLeft(l)),
      (l, m) => SLeft(SRight((l, m))),
      (m, r) => SRight(SLeft((m, r))),
      r => SRight(SRight(r))
    )

  def eitherOption: (Option[M], Either[L, R]) =
    fold(
      left            => (None        , SLeft(left)  ),
      (left, middle)  => (Some(middle), SLeft(left)  ),
      (middle, right) => (Some(middle), SRight(right)),
      right           => (None        , SRight(right))
    )

  def options: (Option[L], Option[M], Option[R]) =
    fold(
      left            => (Some(left), None        , None       ),
      (left, middle)  => (Some(left), Some(middle), None       ),
      (middle, right) => (None      , Some(middle), Some(right)),
      right           => (None      , None        , Some(right))
    )

  def isValid: Boolean     = fold(_ => false, (_, _) => false, (_, _) => false, _ => true )

  def hasLeft: Boolean     = fold(_ => true , (_, _) => true , (_, _) => false, _ => false)
  def hasMiddle: Boolean   = fold(_ => false, (_, _) => true , (_, _) => true , _ => false)
  def hasRight: Boolean    = fold(_ => false, (_, _) => false, (_, _) => true , _ => true )

  def hasNoLeft: Boolean   = fold(_ => false, (_, _) => false, (_, _) => true , _ => true )
  def hasNoMiddle: Boolean = fold(_ => true , (_, _) => false, (_, _) => false, _ => true )
  def hasNoRight: Boolean  = fold(_ => true , (_, _) => true , (_, _) => false, _ => false)
}

object Trior {
  def right[R](right: R): Trior[Nothing, Nothing, R] = Right(right)
  def rightWithMiddle[M, R](middle: M, right: R): Trior[Nothing, M, R] = MidRight(middle, right)

  def left[L](left: L): Trior[L, Nothing, Nothing] = Left(left)
  def leftWithMiddle[L, M](left: L, middle: M): Trior[L, M, Nothing] = MidLeft(left, middle)

  implicit def triorMonad[L, M: Semigroup]: Monad[Trior[L, M, ?]] =
    new Monad[Trior[L, M, ?]] {
      def pure[A](a: A): Trior[L, M, A] = Right(a)
      def map[A, B](fa: Trior[L, M, A])(f: A => B): Trior[L, M, B] = fa.map(f)
      def flatMap[A, B](fa: Trior[L, M, A])(f: A => Trior[L, M, B]): Trior[L, M, B] =
        fa.fold(
          Left(_),
          MidLeft(_, _),
          (m, a) => f(a).mapMiddle(m |+| _),
          f
        )
    }

  implicit def checkedMonoid[L: Semigroup, M: Semigroup, A](implicit monoid: Monoid[A]): Monoid[Trior[L, M, A]] =
    new Monoid[Trior[L, M, A]] {
      def empty: Trior[L, M, A] = Right(monoid.empty)
      def combine(l: Trior[L, M, A], r: Trior[L, M, A]): Trior[L, M, A] = l.combine(r)
    }

}

case class Left[+L, +M](l: L) extends Trior[L, M, Nothing]
case class MidLeft[+L, +M](l: L, m: M) extends Trior[L, M, Nothing]
case class MidRight[+M, +R](m: M, r: R) extends Trior[Nothing, M, R]
case class Right[+R](r: R) extends Trior[Nothing, Nothing, R]

trait TriorSyntax {
  implicit class TriorOps[A](value: A) {
    def tright: Trior[Nothing, Nothing, A] = Right(value)
  }
}
