package checklist

import org.scalatest._
import cats.implicits._

class SizeableSpec extends FreeSpec with Matchers {
  "Sizeable Foldable" in {
    def vectorSizeable[A] = Sizeable.sizeableFoldable[Vector, A]
    vectorSizeable.size(Vector(1,2,3,4)) should be(4)
  }

  "Sizeable Seq" in {
    def seqSizable[A] = Sizeable.sizeableSeq[A]
    seqSizable.size(Seq(1,2,3,4)) should be(4)
  }

  "Sizeable String" in {
    Sizeable.sizeableString.size("foo") should be(3)
  }
}
