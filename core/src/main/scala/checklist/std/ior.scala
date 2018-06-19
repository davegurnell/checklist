package checklist
package std

import cats._
import cats.data.Ior
import checklist.core._

object ior {
  implicit object checked extends Checked[IorMessages] {
    override def pass[A](value: A): IorMessages[A] =
      Ior.right(value)

    override def warn[A](message: String): IorMessages[A] =
      Ior.left(Message.warnings(message))

    override def warn[A](message: String, value: A): IorMessages[A] =
      Ior.both(Message.warnings(message), value)

    override def fail[A](message: String): IorMessages[A] =
      Ior.left(Message.errors(message))

    override def fail[A](message: String, value: A): IorMessages[A] =
      Ior.both(Message.errors(message), value)

    override def prefix[A, P](ior: IorMessages[A], path: P)(implicit prefix: ToPath[P]): IorMessages[A] =
      ior.leftMap(_.map(_.prefix(path)))
  }

  object rules extends BaseRules[IorMessages]
    with ConversionRules[IorMessages]
    with PropertyRules[IorMessages]
    with ApplicativeRules[IorMessages]
    with MonadicRules[IorMessages, IorMessages] {
    protected val checked: Checked[IorMessages] =
      ior.checked

    protected val monad: Monad[IorMessages] =
      Monad[IorMessages]

    protected val applicative: Applicative[IorMessages] =
      monad

    protected val parallel: Parallel[IorMessages, IorMessages] =
      Parallel[IorMessages, IorMessages]
  }

  object syntax extends RuleSyntax[IorMessages, IorMessages] {
    protected val rules: BaseRules[IorMessages] =
      ior.rules

    protected val checked: Checked[IorMessages] =
      ior.checked

    protected val monad: Monad[IorMessages] =
      Monad[IorMessages]

    protected val parallel: Parallel[IorMessages, IorMessages] =
      Parallel[IorMessages, IorMessages]
  }
}
