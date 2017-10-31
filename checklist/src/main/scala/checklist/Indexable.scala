package checklist

trait Indexable[S[_]] {
  def mapWithIndex[A, B](values: S[A])(f: (A, Int) => B): S[B]
  def zipWithIndex[A](values: S[A]): S[(A, Int)] = mapWithIndex(values)((a, i) => (a, i))
}

object Indexable extends IndexableInstances with IndexableSyntax {
  def apply[S[_]](implicit indexable: Indexable[S]): Indexable[S] =
    indexable
}

trait IndexableInstances extends LowPriorityIndexableInstances {

  implicit val listIndexable: Indexable[List] =
    new Indexable[List] {
      def mapWithIndex[A, B](values: List[A])(f: (A, Int) => B) = values.zipWithIndex.map { case (a, i) => f(a, i) }
      override def zipWithIndex[A](values: List[A]): List[(A, Int)] =
        values.zipWithIndex
    }

  implicit val vectorIndexable: Indexable[Vector] =
    new Indexable[Vector] {
      def mapWithIndex[A, B](values: Vector[A])(f: (A, Int) => B) = values.zipWithIndex.map { case (a, i) => f(a, i) }
      override def zipWithIndex[A](values: Vector[A]): Vector[(A, Int)] =
        values.zipWithIndex
    }

  implicit val streamIndexable: Indexable[Stream] =
    new Indexable[Stream] {
      def mapWithIndex[A, B](values: Stream[A])(f: (A, Int) => B) = values.zipWithIndex.map { case (a, i) => f(a, i) }
      override def zipWithIndex[A](values: Stream[A]): Stream[(A, Int)] =
        values.zipWithIndex
    }
}

trait LowPriorityIndexableInstances {
  import cats.Traverse

  // Most of the stuff below is stolen from cats 1.0.0-MF's Traverse. Once this lib is on cats 1.0.0, Indexable can disappear because traverse does all of this.
  implicit def indexableFromTraverse[S[_]: Traverse]: Indexable[S] = {
    new Indexable[S] {
      import cats.data.State
      import cats.implicits._
      def mapWithIndex[A, B](values: S[A])(f: (A, Int) => B): S[B] =
        values.traverse(a => State((s: Int) => (s + 1, f(a, s)))).runA(0).value
    }
  }
}
