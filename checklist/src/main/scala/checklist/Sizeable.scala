package checklist

trait Sizeable[A] {
  def size(a: A): Long
}

object Sizeable extends SizeableInstances with SizeableSyntax {
  def apply[A](implicit sizeable: Sizeable[A]): Sizeable[A] = sizeable

  def instance[A](f: A => Long): Sizeable[A] =
    new Sizeable[A] {
      def size(a: A): Long = f(a)
    }
}

trait SizeableInstances extends LowPrioritySizeableInstances {
  implicit val sizeableString: Sizeable[String] = Sizeable.instance(_.size.toLong)
}

trait LowPrioritySizeableInstances {
  import cats.Foldable
  import cats.implicits._
  implicit def sizeableFoldable[F[_]: Foldable, A]: Sizeable[F[A]] = Sizeable.instance(_.size)
  implicit def sizeableSeq[A]: Sizeable[Seq[A]] = Sizeable.instance(_.size.toLong)
}
