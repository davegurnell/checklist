package checklist

import cats.implicits._
import cats.{Applicative, Traverse}

object IndexableSyntax extends IndexableSyntax

trait IndexableSyntax {
  implicit class IndexableOps[S[_], A](sa: S[A])(implicit indexable: Indexable[S]) {
    def zipWithIndex: S[(A, Int)] = indexable.zipWithIndex(sa)
    def mapWithIndex[B](f: (A, Int) => B): S[B] = indexable.mapWithIndex(sa)(f)

    def traverseWithIndex[F[_]: Applicative, B](f: (A, Int) => F[B])(implicit traverse: Traverse[S]): F[S[B]] =
      indexable.mapWithIndex(sa)(f).sequence
  }
}
