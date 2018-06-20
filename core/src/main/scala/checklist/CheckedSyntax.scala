package checklist

trait CheckedSyntax {
  implicit class CheckedOps[A](value: Checked[A]) {
    def isValid: Boolean =
      value.left.isEmpty

    def hasAValue: Boolean =
      value.right.nonEmpty

    def hasErrors: Boolean =
      value.left.fold(false)(messages => messages.exists(_.isError))

    def hasNoErrors: Boolean =
      value.left.fold(true)(messages => messages.forall(_.isWarning))

    def hasWarnings: Boolean =
      value.left.fold(false)(messages => messages.exists(_.isWarning))

    def hasNoWarnings: Boolean =
      value.left.fold(true)(messages => messages.forall(_.isError))
  }
}
