package checklist

import cats.data.NonEmptyList

trait MessageSyntax {
  def error(str: String): NonEmptyList[Message] =
    NonEmptyList(ErrorMessage(str), Nil)

  def warning(str: String): NonEmptyList[Message] =
    NonEmptyList(WarningMessage(str), Nil)
}
