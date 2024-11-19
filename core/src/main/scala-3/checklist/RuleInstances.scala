package checklist

import cats.Applicative
import cats.arrow.Profunctor
import cats.data.Ior

/** Type class instances for Rule */
trait RuleInstances {
  self: BaseRules =>

  implicit def ruleApplicative[A]: Applicative[[B] =>> Rule[A, B]] =
    new Applicative[[B] =>> Rule[A, B]] {
      def pure[B](value: B): Rule[A, B] =
        Rule.pure(_ => Ior.right(value))

      def ap[B, C](funcRule: Rule[A, B => C])(argRule: Rule[A, B]): Rule[A, C] =
        funcRule.zip(argRule).map { pair =>
          val (func, arg) = pair
          func(arg)
        }

      override def map[B, C](rule: Rule[A, B])(func: B => C): Rule[A, C] =
        rule.map(func)

      override def product[B, C](
                                  rule1: Rule[A, B],
                                  rule2: Rule[A, C]
                                ): Rule[A, (B, C)] =
        rule1.zip(rule2)
    }

  implicit val ruleProfunctor: Profunctor[Rule] =
    new Profunctor[Rule] {
      override def dimap[A, B, C, D](fab: Rule[A, B])(f: C => A)(g: B => D): Rule[C, D] =
        fab.contramap(f).map(g)
    }
}
