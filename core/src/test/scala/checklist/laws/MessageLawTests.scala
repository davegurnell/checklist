package checklist.laws

import cats.kernel.laws.discipline.{EqTests, OrderTests}
import cats.implicits._
import checklist.Message

class MessageLawTests extends CatsSuite {
  checkAll("Message", EqTests[Message].eqv)
  checkAll("Message", OrderTests[Message].order)
}
