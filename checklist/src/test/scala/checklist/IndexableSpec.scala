package checklist

import org.scalatest._
import cats.implicits._
import Indexable._

class IndexableSpec extends FreeSpec with Matchers {
  "List Indexable" in {
    val list = "foo".toList
    listIndexable.zipWithIndex(list) should be(list.zipWithIndex)
    listIndexable.mapWithIndex(list)((a, b) => (a, b)) should be(listIndexable.zipWithIndex(list))
  }

  "Vector Indexable" in {
    val vector = "foo".toVector
    vectorIndexable.zipWithIndex(vector) should be(vector.zipWithIndex)
    vectorIndexable.mapWithIndex(vector)((a, b) => (a, b)) should be(vectorIndexable.zipWithIndex(vector))
  }

  "Stream Indexable" in {
    val stream = "foo".toStream
    streamIndexable.zipWithIndex(stream).take(3).toList should be(stream.zipWithIndex.take(3).toList)
    streamIndexable.mapWithIndex(stream)((a, b) => (a, b)).take(3).toList should be(streamIndexable.zipWithIndex(stream).take(3).toList)
  }

  "Traverse Indexable" in {
    val indexable = indexableFromTraverse[Vector]
    val vector = "foo".toVector
    indexable.zipWithIndex(vector) should be(vector.zipWithIndex)
    indexable.mapWithIndex(vector)((a, b) => (a, b)) should be(indexable.zipWithIndex(vector))
  }

  "Syntax" in {
    import cats.data.NonEmptyList
    val list = NonEmptyList.of('f', 'o', 'o')

    list.zipWithIndex should be(Indexable[NonEmptyList].zipWithIndex(list))
    list.mapWithIndex((a, i) => (a, i)) should be(Indexable[NonEmptyList].mapWithIndex(list)((a, i) => (a, i)))
    list.traverseWithIndex((a, i) => Option((a, i))) should be(list.zipWithIndex.map { case (a, i) => Option((a, i))}.sequence)
  }
}
