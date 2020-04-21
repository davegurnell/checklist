package checklist

object SizeableSyntax extends SizeableSyntax

trait SizeableSyntax {
  implicit class SizeableOps[A](a: A)(implicit sizeable: Sizeable[A]) {
    def size: Long = sizeable.size(a)
  }
}

