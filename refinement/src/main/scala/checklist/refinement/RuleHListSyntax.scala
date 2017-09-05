package checklist.refinement

import shapeless._
import shapeless.ops.hlist.Reverse
import cats.implicits._
import cats.data.Ior
import checklist.{Rule, PathPrefix}

trait RuleHListSyntax {

  implicit class RuleHList[A, B <: HList, Rev <: HList](rule: Rule[A, B])(implicit reverse: Reverse.Aux[B, Rev])  { self =>

    /**
     * Adds a new property to the rule builder to be sanitzed and validated.  You usually want to use the overload which lets you 
     * specify a path, but if you do not want to specify a path, use this one.
     *
     * @param f The function which picks a property of A to test
     * @param newRule The rule with which to verify the property
     * @tparam C The raw, unvalidated type which is pulled out of an A
     * @tparam D The validated type, produced by running sanitization/validation provided by a `Rule[C, D]`
     */
    def check[C, D](f: A => C)(newRule: Rule[C, D]): Rule[A, D :: B] =
      (newRule.contramap(f) |@| self.rule).map( _ :: _ )

    /**
     * Adds a new property to the rule builder to be sanitzed and validated
     *
     * @param path The path to the property specified, for more helpful errors. Usually a String.
     * @param f The function which picks a property of [[A]] to test
     * @param newRule The rule with which to verify the property  
     * @tparam C The raw, unvalidated type which is pulled out of an [[A]]
     * @tparam D The validated type, produced by running sanitization/validation provided by a `Rule[C, D]`
     * @tparam E The type of the provided `PathPrefix`, usually a `String`.
     */
    def check[C, D, E: PathPrefix](path: E, f: A => C)(newRule: Rule[C, D]): Rule[A, D :: B] =
      (newRule.contramapPath(path)(f) |@| self.rule).map( _ :: _ )

    /**
     * Adds a new property to the rule builder, using the provided input type.  Performs no sanitization/validation.
     *
     * @param f The function for getting a property from [[A]]
     * @tparam C The type to be pulled from [[A]]
     */
    def pass[C](f: A => C): Rule[A, C :: B] =
      check(f)(Rule.pass)

    /**
     * Adds a new property to the rule builder with an arbitrary value.
     *
     * @param c The value of the property to be added
     * @tparam C The type of the property to be added
     */
    def append[C](c: => C): Rule[A, C :: B] =
      (Rule.pure[A, C](_ => Ior.right(c)) |@| self.rule).map( _ :: _ )
    
    /**
     * Finalizes the rule builder, and produces a more useful class as output, rather than an HList.
     *
     * @tparam C The desired rule output type
     */
    def build[C](implicit generic: Generic.Aux[C, Rev]): Rule[A, C] =
      rule.map(b => generic.from(b.reverse))
  }

  implicit class RuleObjectOps(rule: Rule.type) {
    /**
     * Initializes a new [[Rule]] in a valid format for usage with RuleHList syntax.
     *
     * @tparam A The type to be validated
     */
    def builder[A]: Rule[A, HNil] = Rule.pass[A].map(_ => HNil)
  }
}
