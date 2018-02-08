package checklist.laws

import cats.kernel.laws.discipline.MonadTests
import cats.tests.CatsSuite
import checklist.Checked

class PathLawTests extends CatsSuite {
  checkAll("Path", MonadTests[Checked[Int, Int, ?]].order)
}
