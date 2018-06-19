package checklist
package std
package laws

import cats.kernel.laws.discipline.{EqTests, OrderTests}
import cats.tests.CatsSuite

class MessageLawTests extends CatsSuite {
  checkAll("Message", EqTests[Message].eqv)
  checkAll("Message", OrderTests[Message].order)
}
