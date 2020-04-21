package checklist.laws

import org.scalactic.anyvals.{PosInt, PosZDouble, PosZInt}
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

trait TestSettings extends Configuration with Matchers {
  lazy val checkConfiguration: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = PosInt(50),
      maxDiscardedFactor = PosZDouble(5.0),
      minSize = PosZInt(0),
      sizeRange = PosZInt(10),
      workers = PosInt(2)
    )
}

/**
  * An opinionated stack of traits to improve consistency and reduce
  * boilerplate in Cats tests.
  */
trait CatsSuite extends AnyFunSuiteLike with Matchers with FunSuiteDiscipline with TestSettings {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration

  def even(i: Int): Boolean = i % 2 == 0

  val evenPf: PartialFunction[Int, Int] = { case i if even(i) => i }
}
