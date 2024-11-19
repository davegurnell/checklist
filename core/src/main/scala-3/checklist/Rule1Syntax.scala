package checklist

import monocle.Lens

trait Rule1Syntax {
  implicit class AnyRuleOps[A](value: A) {
    def validate(implicit rule: Rule[A, A]): Checked[A] =
      rule(value)
  }

  implicit class Rule1Ops[A](self: Rule[A, A]) {
    def field[B](path: Path, lens: Lens[A, B])(implicit rule: Rule[B, B]): Rule[A, A] =
      self.andThen(rule.at(path, lens))

    inline def field[B](inline accessor: A => B)(implicit inline rule: Rule[B, B]): Rule[A, A] =
      ${ RuleMacros.field[A, B]('self, 'accessor, 'rule) }

    def fieldWith[B](path: Path, lens: Lens[A, B])(implicit builder: A => Rule[B, B]): Rule[A, A] =
      self.andThen(Rule.pure(value => builder(value).at(path, lens).apply(value)))

    inline def fieldWith[B](inline accessor: A => B)(implicit inline builder: A => Rule[B, B]): Rule[A, A] =
      ${ RuleMacros.fieldWith[A, B]('self, 'accessor, 'builder) }
  }
}
