package checklist
package core

/**
 * Rules that  convert one type to another.
 */
trait ConversionRules[F[_]] {
  self: BaseRules[F] =>

  val parseInt: Rule[F, String, Int] =
    parseInt(fatal("Must be a whole number"))

  def parseInt(orElse: Rule[F, String, Int]): Rule[F, String, Int] =
    pure { value =>
      util.Try(value.toInt) match {
        case util.Success(i) => checked.pass(i)
        case util.Failure(_) => orElse(value)
      }
    }

  val parseDouble: Rule[F, String, Double] =
    parseDouble(fatal("Must be a number"))

  def parseDouble(orElse: Rule[F, String, Double]): Rule[F, String, Double] =
    pure { value =>
      util.Try(value.toDouble) match {
        case util.Success(d) => checked.pass(d)
        case util.Failure(_) => orElse(value)
      }
    }

  val trimString: Rule[F, String, String] =
    pure(value => checked.pass(value.trim))
}
