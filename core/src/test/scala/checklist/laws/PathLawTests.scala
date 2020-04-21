package checklist.laws

import cats.kernel.laws.discipline.{MonoidTests, OrderTests}
import cats.implicits._
import checklist.Path

class PathLawTests extends CatsSuite {
  checkAll("Path", OrderTests[Path].order)
  checkAll("Path", MonoidTests[Path].monoid)
}
