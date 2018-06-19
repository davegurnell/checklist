package checklist.std.laws

import cats.kernel.laws.discipline.{MonoidTests, OrderTests}
import cats.tests.CatsSuite
import checklist.Path

class PathLawTests extends CatsSuite {
 checkAll("Path", OrderTests[Path].order)
 checkAll("Path", MonoidTests[Path].monoid)
}
