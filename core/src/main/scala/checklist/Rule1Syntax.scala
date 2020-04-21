package checklist

import monocle._

trait Rule1Syntax {
  implicit class AnyRuleOps[A](value: A) {
    def validate(implicit rule: Rule[A, A]): Checked[A] =
      rule(value)
  }

  implicit class Rule1Ops[A](self: Rule[A, A]) {
    def field[B](path: Path, lens: Lens[A, B])(
        implicit rule: Rule[B, B]
    ): Rule[A, A] =
      self andThen rule.at(path, lens)

    def field[B](accessor: A => B)(implicit rule: Rule[B, B]): Rule[A, A] =
      macro RuleMacros.field[A, B]

    def fieldWith[B](path: Path, lens: Lens[A, B])(
        implicit builder: A => Rule[B, B]
    ): Rule[A, A] =
      self andThen Rule.pure(value =>
        builder(value).at(path, lens).apply(value)
      )

    def fieldWith[B](
        accessor: A => B
    )(implicit builder: A => Rule[B, B]): Rule[A, A] =
      macro RuleMacros.fieldWith[A, B]
  }
}
