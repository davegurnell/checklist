package checklist
package core

import cats.{Monad, Parallel}
import monocle.{Lens, PLens}

import scala.language.experimental.macros

trait RuleSyntax[F[_], G[_]] {
  protected def rules: BaseRules[F]

  implicit protected def checked: Checked[F]
  implicit protected def monad: Monad[F]
  implicit protected def parallel: Parallel[F, G]

  implicit class AnyRuleOps[A](value: A) {
    def validate(implicit rule: Rule[F, A, A]): F[A] =
      rule(value)

    def validateAs[B](implicit rule: Rule[F, A, B]): F[B] =
      rule(value)
  }

  implicit class RuleOps[A, B](rule: Rule[F, A, B]) {
    def prefix[P: ToPath](path: P): Rule[F, A, B] =
      rules.pure(value => checked.prefix(rule(value), path))

    def composeLens[S, T](lens: PLens[S, T, A, B]): Rule[F, S, T] =
      rules.pure(value => rule.map(lens.set(_)(value)).apply(lens.get(value)))

    def at[P, S, T](path: P, lens: PLens[S, T, A, B])(implicit toPath: ToPath[P]): Rule[F, S, T] =
      rule.composeLens(lens).prefix(path)
  }

  implicit class Rule1Ops[A](rule1: Rule[F, A, A]) {
    def field[B](path: Path, lens: Lens[A, B])(implicit rule2: Rule[F, B, B]): Rule[F, A, A] =
      rule1.andThen(new RuleOps(rule2).at(path, lens))

    def field[B](accessor: A => B)(implicit rule2: Rule[F, B, B]): Rule[F, A, A] =
      macro RuleMacros.field[A, B]

    def fieldWith[B](path: Path, lens: Lens[A, B])(implicit builder: A => Rule[F, B, B]): Rule[F, A, A] =
      rule1.andThen(rules.pure((value: A) => builder(value).at(path, lens).apply(value)))

    def fieldWith[B](accessor: A => B)(implicit builder: A => Rule[F, B, B]): Rule[F, A, A] =
      macro RuleMacros.fieldWith[A, B]
  }
}
