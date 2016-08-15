package checklist
package update

import cats._
import cats.data._
import cats.instances.all._
import cats.syntax.all._
import monocle.{PLens, Lens}
import scala.language.higherKinds
import Message.{errors, warnings}

sealed abstract class UpdateRule[A, B] {
  def apply(original: A, updated: A): Checked[B]

  def map[C](func: B => C): UpdateRule[A, C] =
    UpdateRule.pure[A, C]((original, updated) => this(original, updated) map func)

  def flatMap[C](func: B => UpdateRule[A, C]): UpdateRule[A, C] =
    UpdateRule.pure[A, C]((original, updated) => this(original, updated) flatMap (func(_)(original, updated)))

  def zip[C](that: UpdateRule[A, C]): UpdateRule[A, (B, C)] =
    UpdateRule.pure[A, (B, C)] { (original, updated) =>
      this(original, updated) match {
        case Ior.Left(msg1) =>
          that(original, updated) match {
            case Ior.Left(msg2)    => Ior.left(msg1 concat msg2)
            case Ior.Right(c)      => Ior.left(msg1)
            case Ior.Both(msg2, c) => Ior.left(msg1 concat msg2)
          }
        case Ior.Right(b) =>
          that(original, updated) match {
            case Ior.Left(msg2)    => Ior.left(msg2)
            case Ior.Right(c)      => Ior.right((b, c))
            case Ior.Both(msg2, c) => Ior.both(msg2, (b, c))
          }
        case Ior.Both(msg1, b) =>
          that(original, updated) match {
            case Ior.Left(msg2)    => Ior.left(msg1 concat msg2)
            case Ior.Right(c)      => Ior.both(msg1, (b, c))
            case Ior.Both(msg2, c) => Ior.both(msg1 concat msg2, (b, c))
          }
      }
    }

  // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
  def prefix[P: PathPrefix](prefix: P): UpdateRule[A, B] =
    UpdateRule.pure((original, updated) => this(original, updated) leftMap (_ map (_ prefix prefix)))

  // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
  def composeLens[S, T](lens: PLens[S, T, A, B]): UpdateRule[S, T] =
    UpdateRule.pure((original, updated) => this(lens.get(original), lens.get(updated)) map (lens.set(_)(original)))

  // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
  def at[P: PathPrefix, S, T](prefix: P, lens: PLens[S, T, A, B]): UpdateRule[S, T] =
    this composeLens lens prefix prefix
}

object UpdateRule extends BaseUpdateRules
  with PropertyUpdateRules
  with UpdateRule1Syntax

trait BaseUpdateRules {
  def pure[A, B](func: (A, A) => Checked[B]): UpdateRule[A, B] =
    new UpdateRule[A, B] {
      def apply(original: A, updated: A) =
        func(original, updated)
    }
}

trait PropertyUpdateRules {
  def alwaysReplace[A, B >: A]: UpdateRule[A, B] =
    UpdateRule.pure[A, B]((original, updated) => Ior.right(updated))

  def alwaysIgnore[A, B >: A]: UpdateRule[A, B] =
    UpdateRule.pure[A, B]((original, updated) => Ior.right(original))

  def cannotChange[A, B >: A]: UpdateRule[A, B] =
    UpdateRule.cannotChange(errors("Cannot be changed"))

  def cannotChange[A, B >: A](messages: Messages): UpdateRule[A, B] =
    UpdateRule.pure[A, B] { (original, updated) =>
      if(original == updated) {
        Ior.right(updated)
      } else {
        Ior.both(messages, original)
      }
    }

  // TODO: Find a nicer syntax for this.
  // We need the empty parens here to avoid having to resolve ambiguity
  // by annotating function parameter types.
  def canChangeTo[A, B >: A]()(test: B => Boolean): UpdateRule[A, B] =
    UpdateRule.canChangeTo(errors("Cannot be changed to that value"))(test)

  def canChangeTo[A, B >: A](messages: Messages)(test: B => Boolean): UpdateRule[A, B] =
    UpdateRule.pure[A, B] { (original, updated) =>
      if(original == updated || test(updated)) {
        Ior.right(updated)
      } else {
        Ior.both(messages, original)
      }
    }
}

/* Per-field validation syntax for existing data */
trait UpdateRule1Syntax {
  import scala.language.experimental.macros

  implicit class UpdateRule1Ops[A](self: UpdateRule1[A]) {
    // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
    def field[B](path: Path, lens: Lens[A, B])(implicit rule: UpdateRule1[B]): UpdateRule1[A] =
      self zip rule.at(path, lens) map (_._1)

    // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
    def field[B](accessor: A => B)(implicit rule: UpdateRule1[B]): UpdateRule1[A] =
      macro UpdateRuleMacros.field[A, B]

    // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
    def fieldWith[B](path: Path, lens: Lens[A, B])(implicit builder: A => UpdateRule1[B]): UpdateRule1[A] =
      self zip UpdateRule.pure((original, updated) => builder(original).at(path, lens).apply(original, updated)) map (_._1)

    // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
    def fieldWith[B](accessor: A => B)(implicit builder: A => UpdateRule1[B]): UpdateRule1[A] =
      macro UpdateRuleMacros.fieldWith[A, B]
  }
}