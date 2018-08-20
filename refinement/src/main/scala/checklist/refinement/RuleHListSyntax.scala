package checklist.refinement

import cats.Applicative
import shapeless._
import shapeless.ops.hlist.Reverse
import cats.implicits._
import cats.data.Ior
import checklist.{PathPrefix, Rule}

import scala.language.higherKinds

trait RuleHListSyntax {

  implicit class RuleHList[F[_] : Applicative, A, B <: HList, Rev <: HList](rule: Rule[F, A, B])(implicit reverse: Reverse.Aux[B, Rev]) {
    self =>

    /**
      * Adds a new property to the rule builder to be sanitzed and validated.  You usually want to use the overload which lets you
      * specify a path, but if you do not want to specify a path, use this one.
      *
      * @param f       The function which picks a property of A to test
      * @param newRule The rule with which to verify the property
      * @tparam C The raw, unvalidated type which is pulled out of an A
      * @tparam D The validated type, produced by running sanitization/validation provided by a `Rule[C, D]`
      */
    def check[C, D](f: A => C)(newRule: Rule[F, C, D]): Rule[F, A, D :: B] =
      (newRule.contramap(f), self.rule).mapN(_ :: _)

    /**
      * Adds a new property to the rule builder to be sanitzed and validated
      *
      * @param path    The path to the property specified, for more helpful errors. Usually a String.
      * @param f       The function which picks a property of `A` to test
      * @param newRule The rule with which to verify the property
      * @tparam C The raw, unvalidated type which is pulled out of an `A`
      * @tparam D The validated type, produced by running sanitization/validation provided by a `Rule[C, D]`
      * @tparam E The type of the provided `PathPrefix`, usually a `String`.
      */
    def check[C, D, E: PathPrefix](path: E, f: A => C)(newRule: Rule[F, C, D]): Rule[F, A, D :: B] =
      (newRule.contramapPath(path)(f), self.rule).mapN(_ :: _)

    /**
      * Checks a property and discards the result.
      *
      * @param path    The path to the property specified, for more helpful errors. Usually a String.
      * @param f       The function which picks a property of `A` to test
      * @param newRule The rule with which to verify the property
      * @tparam P The type of the provided `PathPrefix`, usually a `String`
      * @tparam C The raw, unvalidated type which is pulled out of an `A`
      * @tparam D The return type of `newRule`, which will actually be discarded (used internally for unification).
      */
    def checkAndDrop[P: PathPrefix, C, D](path: P, f: A => C)(rule: Rule[F, C, D]): Rule[F, A, B] =
      (Rule.pure[F, A, D] { a: A => rule(f(a)) }).prefix(path) *> self.rule

    /**
      * Checks a property and discards the result.
      *
      * @param f       The function which picks a property of `A` to test
      * @param newRule The rule with which to verify the property
      * @tparam C The raw, unvalidated type which is pulled out of an `A`
      * @tparam D The return type of `newRule`, which will actually be discarded (used internally for unification).
      */
    def checkAndDrop[C, D](f: A => C)(rule: Rule[F, C, D]): Rule[F, A, B] =
      (Rule.pure[F, A, D]  { a: A => rule(f(a)) }) *> self.rule

    /**
      * Adds a new property to the rule builder, using the provided input type.  Performs no sanitization/validation.
      *
      * @param f The function for getting a property from `A`
      * @tparam C The type to be pulled from `A`
      */
    def pass[C](f: A => C): Rule[F, A, C :: B] =
      check(f)(Rule.apply)

    /**
      * Adds a new property to the rule builder with an arbitrary value.
      *
      * @param c The value of the property to be added
      * @tparam C The type of the property to be added
      */
    def append[C](c: => C): Rule[F, A, C :: B] =
      (Rule.pure[F, A, C](_ => Applicative[F].pure(Ior.right(c))), self.rule).mapN(_ :: _)

    /**
      * Finalizes the rule builder, and produces a more useful class as output, rather than an HList.
      *
      * @tparam C The desired rule output type
      */
    def build[C](implicit generic: Generic.Aux[C, Rev]): Rule[F, A, C] =
      rule.map(b => generic.from(b.reverse))
  }

  implicit class RuleObjectOps(rule: Rule.type) {
    /**
      * Initializes a new `Rule` in a valid format for usage with RuleHList syntax.
      *
      * @tparam A The type to be validated
      */
    def builder[F[_] : Applicative, A]: Rule[F, A, HNil] = Rule.apply[F, A].map(_ => HNil)
  }

}
