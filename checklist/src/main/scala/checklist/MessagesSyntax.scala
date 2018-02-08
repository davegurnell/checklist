package checklist

import cats.data.NonEmptyList

trait MessageSyntax {
  def error(str: String): Checked[Nothing] =
    Trior.left(NonEmptyList(ErrorMessage(str), Nil))

  def errors(str: String): Checked[Nothing] =
    Trior.left(NonEmptyList(ErrorMessage(str), Nil))

  def warning[A](result: A, message: String): Checked[A] =
    Trior.leftWithMiddle(NonEmptyList(WarningMessage(str), Nil), result)

  def warnings[A](result: A, message: String, messages: String*): Checked[A] =
    Trior.leftWithMiddle(NonEmptyList.of(WarningMessage(str, Nil), messages.map(WarningMessage(_, Nil)):_ *), result)
}
