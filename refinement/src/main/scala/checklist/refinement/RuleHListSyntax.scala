package checklist.refinement

import shapeless._
import shapeless.ops.hlist.Reverse
import cats.implicits._
import checklist.{Rule, PathPrefix}

trait RuleHListSyntax {

  implicit class RuleHList[A, B <: HList, Rev <: HList](rule: Rule[A, B])(implicit reverse: Reverse.Aux[B, Rev])  { self =>

    def check[C, D](f: A => C)(newRule: Rule[C, D]): Rule[A, D :: B] =
      (newRule.contramap(f) |@| self.rule).map( _ :: _ )

    def check[C, D, E: PathPrefix](path: E, f: A => C)(newRule: Rule[C, D]): Rule[A, D :: B] =
      (newRule.contramapPath(path)(f) |@| self.rule).map( _ :: _ )

    def build[C](implicit generic: Generic.Aux[C, Rev]): Rule[A, C] =
      rule.map(b => generic.from(b.reverse))
  }

  implicit class RuleObjectOps(rule: Rule.type) {
    def builder[A]: Rule[A, HNil] = Rule.pass[A].map(_ => HNil)
  }
}
