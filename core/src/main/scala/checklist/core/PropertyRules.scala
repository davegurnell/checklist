package checklist
package core

import cats._
import cats.data.NonEmptyList

import scala.util.matching.Regex

/**
 * Rules that test a property of an existing value.
 */
trait PropertyRules[F[_]] {
  self: BaseRules[F] =>

  def test[A](orElse: Rule[F, A, A])(func: A => Boolean): Rule[F, A, A] =
    pure(value => if(func(value)) checked.pass(value) else orElse(value))

  def eql[A](comp: A): Rule[F, A, A] =
    eql(comp, fail(s"Must be $comp"))

  def eql[A](comp: A, orElse: Rule[F, A, A]): Rule[F, A, A] =
    test(orElse)(_ == comp)

  def neq[A](comp: A): Rule[F, A, A] =
    neq(comp, fail(s"Must not be $comp"))

  def neq[A](comp: A, orElse: Rule[F, A, A]): Rule[F, A, A] =
    test(orElse)(_ != comp)

  def gt[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[F, A, A] =
    gt(comp, fail(s"Must be greater than $comp"))

  def gt[A](comp: A, orElse: Rule[F, A, A])(implicit ord: Ordering[_ >: A]): Rule[F, A, A] =
    test(orElse)(ord.gt(_, comp))

  def lt[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[F, A, A] =
    lt(comp, fail(s"Must be less than $comp"))

  def lt[A](comp: A, orElse: Rule[F, A, A])(implicit ord: Ordering[_ >: A]): Rule[F, A, A] =
    test(orElse)(ord.lt(_, comp))

  def gte[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[F, A, A] =
    gte(comp, fail(s"Must be greater than or equal to $comp"))

  def gte[A](comp: A, orElse: Rule[F, A, A])(implicit ord: Ordering[_ >: A]): Rule[F, A, A] =
    test(orElse)(ord.gteq(_, comp))

  def lte[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[F, A, A] =
    lte(comp, fail(s"Must be less than or equal to $comp"))

  def lte[A](comp: A, orElse: Rule[F, A, A])(implicit ord: Ordering[_ >: A]): Rule[F, A, A] =
    test(orElse)(ord.lteq(_, comp))

  def nonEmpty[A](implicit monoid: Monoid[A]): Rule[F, A, A] =
    nonEmpty(fail[A](s"Must not be empty"))

  def nonEmpty[A](orElse: Rule[F, A, A])(implicit monoid: Monoid[A]): Rule[F, A, A] =
    test(orElse)(value => value != monoid.empty)

  def lengthEq[A](comp: Int)(implicit sizeable: Sizeable[A]): Rule[F, A, A] =
    lengthEq(comp, fail(s"Must be length $comp or greater"))

  def lengthEq[A](comp: Int, orElse: Rule[F, A, A])(implicit sizeable: Sizeable[A]): Rule[F, A, A] =
    test(orElse)(value => sizeable.size(value) == comp)

  def lengthLt[A](comp: Int)(implicit sizeable: Sizeable[A]): Rule[F, A, A] =
    lengthLt(comp, fail(s"Must be length $comp or greater"))

  def lengthLt[A](comp: Int, orElse: Rule[F, A, A])(implicit sizeable: Sizeable[A]): Rule[F, A, A] =
    test(orElse)(value => sizeable.size(value) < comp)

  def lengthGt[A](comp: Int)(implicit sizeable: Sizeable[A]): Rule[F, A, A] =
    lengthGt(comp, fail(s"Must be length $comp or shorter"))

  def lengthGt[A](comp: Int, orElse: Rule[F, A, A])(implicit sizeable: Sizeable[A]): Rule[F, A, A] =
    test(orElse)(value => sizeable.size(value) > comp)

  def lengthLte[A](comp: Int)(implicit sizeable: Sizeable[A]): Rule[F, A, A] =
    lengthLte(comp, fail(s"Must be length $comp or greater"))

  def lengthLte[A](comp: Int, orElse: Rule[F, A, A])(implicit sizeable: Sizeable[A]): Rule[F, A, A] =
    test(orElse)(value => sizeable.size(value) <= comp)

  def lengthGte[A](comp: Int)(implicit sizeable: Sizeable[A]): Rule[F, A, A] =
    lengthGte(comp, fail(s"Must be length $comp or shorter"))

  def lengthGte[A](comp: Int, orElse: Rule[F, A, A])(implicit sizeable: Sizeable[A]): Rule[F, A, A] =
    test(orElse)(value => sizeable.size(value) >= comp)

  def nonEmptyList[A]: Rule[F, List[A], NonEmptyList[A]] =
    nonEmptyList(fatal[List[A], NonEmptyList[A]]("Must not be empty"))

  def nonEmptyList[A](orElse: Rule[F, List[A], NonEmptyList[A]]): Rule[F, List[A], NonEmptyList[A]] =
    pure {
      case Nil    => orElse(Nil)
      case h :: t => checked.pass(NonEmptyList(h, t))
    }

  def matchesRegex(regex: Regex): Rule[F, String, String] =
    matchesRegex(regex, fail(s"Must match the pattern '$regex'"))

  def matchesRegex(regex: Regex, orElse: Rule[F, String, String]): Rule[F, String, String] =
    test(orElse)(regex.findFirstIn(_).isDefined)

  def containedIn[A](values: Seq[A]): Rule[F, A, A] =
    containedIn(values, fail(s"Must be one of the values ${values.mkString(", ")}"))

  def containedIn[A](values: Seq[A], orElse: Rule[F, A, A]): Rule[F, A, A] =
    test(orElse)(value => values contains value)

  def notContainedIn[A](values: Seq[A]): Rule[F, A, A] =
    notContainedIn(values, fail(s"Must not be one of the values ${values.mkString(", ")}"))

  def notContainedIn[A](values: Seq[A], orElse: Rule[F, A, A]): Rule[F, A, A] =
    test(orElse)(value => !(values contains value))
}
