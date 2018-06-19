package checklist

import monocle.PLens

trait Checked[F[_]] {
  def pass[A](value: A): F[A]

  def warn[A](message: String): F[A]
  def warn[A](message: String, value: A): F[A]

  def fail[A](message: String): F[A]
  def fail[A](message: String, value: A): F[A]

  def prefix[A, P](checked: F[A], path: P)(implicit prefix: ToPath[P]): F[A]
}
