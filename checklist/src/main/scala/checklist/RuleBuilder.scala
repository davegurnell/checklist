package checklist

import shapeless._
import cats.implicits._

trait RuleBuilder[A, B <: HList] { self =>

  protected def rule: Rule[A, B]

  def field[C, D](f: A => C)(newRule: Rule[C, D]): RuleBuilder[A, D :: B] =
    new RuleBuilder[A, D :: B] {
      protected def rule: Rule[A, D :: B] =
        (newRule.contramap[A](f) |@| self.rule).map( _ :: _ )
    }

  def build[C](implicit generic: Generic.Aux[B, C]): Rule[A, C] =
    rule.map(generic.to)
}

object RuleBuilder {
  def apply[A]: RuleBuilder[A, HNil] = apply[A](Rule.pass[A])

  def apply[A](baseRule: Rule[A, A]): RuleBuilder[A, HNil] =
    new RuleBuilder[A, HNil] {
      protected def rule = baseRule.map(_ => HNil)
    }
}

