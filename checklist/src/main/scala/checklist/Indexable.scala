package checklist

import scala.language.higherKinds

trait Indexable[S[_]] {
  def zipWithIndex[A](values: S[A]): S[(A, Int)]
}

object Indexable {
  def apply[S[_]](implicit indexable: Indexable[S]): Indexable[S] =
    indexable

  implicit val listIndexable: Indexable[List] =
    new Indexable[List] {
      def zipWithIndex[A](values: List[A]): List[(A, Int)] =
        values.zipWithIndex
    }

  implicit val vectorIndexable: Indexable[Vector] =
    new Indexable[Vector] {
      def zipWithIndex[A](values: Vector[A]): Vector[(A, Int)] =
        values.zipWithIndex
    }

  implicit val streamIndexable: Indexable[Stream] =
    new Indexable[Stream] {
      def zipWithIndex[A](values: Stream[A]): Stream[(A, Int)] =
        values.zipWithIndex
    }
}