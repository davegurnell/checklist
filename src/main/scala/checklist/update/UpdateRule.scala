package checklist
package update

import cats._
import cats.data._
import cats.instances.all._
import cats.syntax.all._
import monocle.{PLens, Lens}
import scala.language.higherKinds
import Message.{errors, warnings}

sealed abstract class UpdateRule[A, B, C] extends Serializable {
  def apply(original: A, update: B): Checked[C]

  def map[D](func: C => D): UpdateRule[A, B, D] =
    UpdateRule.pure[A, B, D]((original, update) => this(original, update) map func)

  def flatMap[D](func: C => UpdateRule[A, B, D]): UpdateRule[A, B, D] =
    UpdateRule.pure[A, B, D]((original, update) => this(original, update) flatMap (func(_)(original, update)))

  def andThen[D](that: UpdateRule[C, B, D]): UpdateRule[A, B, D] =
    UpdateRule.pure[A, B, D]((original, update) => this(original, update) flatMap (that.apply(_, update)))

  def zip[D](that: UpdateRule[A, B, D]): UpdateRule[A, B, (C, D)] =
    UpdateRule.pure[A, B, (C, D)] { (original, update) =>
      this(original, update) match {
        case Ior.Left(msg1) =>
          that(original, update) match {
            case Ior.Left(msg2)    => Ior.left(msg1 concat msg2)
            case Ior.Right(c)      => Ior.left(msg1)
            case Ior.Both(msg2, c) => Ior.left(msg1 concat msg2)
          }
        case Ior.Right(b) =>
          that(original, update) match {
            case Ior.Left(msg2)    => Ior.left(msg2)
            case Ior.Right(c)      => Ior.right((b, c))
            case Ior.Both(msg2, c) => Ior.both(msg2, (b, c))
          }
        case Ior.Both(msg1, b) =>
          that(original, update) match {
            case Ior.Left(msg2)    => Ior.left(msg1 concat msg2)
            case Ior.Right(c)      => Ior.both(msg1, (b, c))
            case Ior.Both(msg2, c) => Ior.both(msg1 concat msg2, (b, c))
          }
      }
    }

  // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
  def prefix[P: PathPrefix](prefix: P): UpdateRule[A, B, C] =
    UpdateRule.pure((original, update) => this(original, update) leftMap (_ map (_ prefix prefix)))

  // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
  def composeLens[S, T](lens: PLens[S, T, A, C]): UpdateRule[S, B, T] =
    UpdateRule.pure((original, update) => this(lens.get(original), update) map (lens.set(_)(original)))

  // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
  def at[P: PathPrefix, S, T](prefix: P, lens: PLens[S, T, A, C]): UpdateRule[S, B, T] =
    this composeLens lens prefix prefix
}

object UpdateRule extends BaseUpdateRules
  with PropertyUpdateRules
  with UpdateRule1Syntax

trait BaseUpdateRules {
  def pure[A, B, C](func: (A, B) => Checked[C]): UpdateRule[A, B, C] =
    new UpdateRule[A, B, C] {
      def apply(original: A, update: B) =
        func(original, update)
    }
}

trait PropertyUpdateRules {
  def alwaysReplace[A, B >: A]: UpdateRule[A, B, B] =
    UpdateRule.pure[A, B, B]((original, update) => Ior.right(update))

  def alwaysIgnore[A, B >: A]: UpdateRule[A, B, A] =
    UpdateRule.pure[A, B, A]((original, update) => Ior.right(original))

  def cannotChange[A, B >: A]: UpdateRule[A, B, B] =
    UpdateRule.cannotChange(errors("Cannot be changed"))

  def cannotChange[A, B >: A](messages: Messages): UpdateRule[A, B, B] =
    UpdateRule.pure[A, B, B] { (original, update) =>
      if(original == update) {
        Ior.right(update)
      } else {
        Ior.both(messages, original)
      }
    }

  // TODO: Find a nicer syntax for this.
  // We need the empty parens here to avoid having to resolve ambiguity
  // by annotating function parameter types.
  def canChangeTo[A, B >: A]()(test: B => Boolean): UpdateRule[A, B, B] =
    UpdateRule.canChangeTo(errors("Cannot be changed to that value"))(test)

  def canChangeTo[A, B >: A](messages: Messages)(test: B => Boolean): UpdateRule[A, B, B] =
    UpdateRule.pure[A, B, B] { (original, update) =>
      if(original == update || test(update)) {
        Ior.right(update)
      } else {
        Ior.both(messages, original)
      }
    }
}

/* Per-field validation syntax for existing data */
trait UpdateRule1Syntax {
  import scala.language.experimental.macros

  implicit class UpdateRule1Ops[A, B](self: UpdateRule[A, B, A]) {
    // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
    def field[C](path: Path, lens: Lens[A, C])(implicit rule: UpdateRule[C, B, C]): UpdateRule[A, B, A] =
      self andThen rule.at(path, lens)

    // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
    def field[B](accessor: A => B)(implicit rule: UpdateRule1[B]): UpdateRule1[A] =
      macro UpdateRuleMacros.field[A, B]

    // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
    def fieldWith[C](path: Path, lens: Lens[A, C])(implicit builder: A => UpdateRule[C, B, C]): UpdateRule[A, B, A] =
      self andThen UpdateRule.pure((original, update) => builder(original).at(path, lens).apply(original, update))

    // TODO: Write unit tests. And be prepared. There's a high chance this is all horribly wrong.
    def fieldWith[B](accessor: A => B)(implicit builder: A => UpdateRule1[B]): UpdateRule1[A] =
      macro UpdateRuleMacros.fieldWith[A, B]
  }
}