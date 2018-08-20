package checklist

import cats.Applicative
import monocle._

import scala.language.experimental.macros
import scala.language.higherKinds

trait CheckSyntax {

  implicit class AnyRuleOps[A](value: A) {
    def validate[F[_] : Applicative](implicit rule: Rule[F, A, A]): F[Checked[A]] =
      rule(value)
  }

  implicit def fieldPartialToRule[F[_], G[_], R[_], A, B](self: CheckOps[F, A]#FieldPartial[B])(implicit rule: Check[G, B], canLift: CanLift[Applicative, F, G, R], evG: Applicative[G]): Rule[R, A, A] = {
    self.apply
  }

  implicit class CheckOps[F[_] : Applicative, A](self: Check[F, A]) {
    def and[G[_] : Applicative, R[_]](that: Check[G, A])(implicit canLift: CanLift[Applicative, F, G, R]): Check[R, A] =
      (self zip that).map(_._1) // Unless we define different semantics for Check, this seems to be the best we can do.
    // Well, we could also check for equality at runtime and throw an exception or warning, but it doesn't seem like the right thing to do.

    def field[G[_] : Applicative, R[_], B](path: Path, lens: Lens[A, B])(rule: Check[G, B])(implicit canLift: CanLift[Applicative, F, G, R]): Check[R, A] =
      self and rule.at(path, lens)

    def field[G[_] : Applicative, R[_], B](accessor: A => B)(rule: Check[G, B])(implicit canLift: CanLift[Applicative, F, G, R]): Check[R, A] =
    macro RuleMacros.field[F, G, R, A, B]

    class FieldPartial[B](path: Path, lens: Lens[A, B]) {
      def apply[G[_], R[_]](implicit rule: Check[G, B], canLift: CanLift[Applicative, F, G, R], evG: Applicative[G]): Check[R, A] =
        field(path, lens)(rule)
    }

    def fieldImplicit[B](path: Path, lens: Lens[A, B]): FieldPartial[B] = {
      new FieldPartial[B](path, lens)
    }

    def fieldImplicit[B](accessor: A => B): FieldPartial[B] =
    macro RuleMacros.fieldImplicit[F, A, B]

    // This is not really working, the compiler can't unify. We should try with embedded class with apply().
    //    def fieldImplicit[G[_] : Applicative, R[_], B](path: Path, lens: Lens[A, B])(implicit rule: Rule[G, B, B], canLift: CanLift[Applicative, F, G, R]): Rule[R, A, A] =
    //      field(path, lens)(rule)

    //    def fieldImplicit[G[_] : Applicative, R[_], B](accessor: A => B)(implicit rule: Rule[G, B, B], canLift: CanLift[Applicative, F, G, R]): Rule[R, A, A] =
    //    macro RuleMacros.fieldImplicit[F, G, R, A, B]

    // This approach is a bit better, but still the compiler requests that we explicit the
    // A type in the accessor, which is not very elegant (see ReadmeSpec line 34)
    def fieldWith[G[_] : Applicative, R[_], B](path: Path, lens: Lens[A, B])(builder: A => Check[G, B])(implicit canLift: CanLift[Applicative, F, G, R]): Check[R, A] =
      self and Rule.pure(value => builder(value).at(path, lens).apply(value))

    def fieldWith[G[_] : Applicative, R[_], B](accessor: A => B)(builder: A => Check[G, B])(implicit canLift: CanLift[Applicative, F, G, R]): Check[R, A] =
    macro RuleMacros.fieldWith[F, G, R, A, B]

    def fieldWithImplicit[G[_] : Applicative, R[_], B](path: Path, lens: Lens[A, B])(implicit builder: A => Check[G, B], canLift: CanLift[Applicative, F, G, R]): Check[R, A] =
      fieldWith(path, lens)(builder)

    def fieldWithImplicit[G[_] : Applicative, R[_], B](accessor: A => B)(implicit builder: A => Check[G, B], canLift: CanLift[Applicative, F, G, R]): Check[R, A] =
    macro RuleMacros.fieldWithImplicit[F, G, R, A, B]
  }

}
