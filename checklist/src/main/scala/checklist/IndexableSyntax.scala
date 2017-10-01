package checklist

import cats.implicits._
import cats.{Applicative, Traverse}

object IndexableSyntax extends IndexableSyntax

trait IndexableSyntax {
  implicit class IndexableOps2[S[_], A](sa: S[A]) {
    def zipWithIndex(implicit indexable: Indexable[S]): S[(A, Int)] = indexable.zipWithIndex(sa)
    def mapWithIndex[B](f: (A, Int) => B)(implicit indexable: Indexable[S]): S[B] = indexable.mapWithIndex(sa)(f)

    def traverseWithIndex[F[_]: Applicative, B](f: (A, Int) => F[B])(implicit traverse: Traverse[S], indexable: Indexable[S]): F[S[B]] =
      indexable.mapWithIndex(sa)(f).sequence
  }
}
