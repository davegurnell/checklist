package checklist.std
package laws

import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{ApplicativeTests, ProfunctorTests}
import cats.tests.CatsSuite
import checklist._

class RuleLawTests extends CatsSuite {
 checkAll("Rule[IorMessages, Int, String]", ApplicativeTests[Rule[IorMessages, Int, ?]].applicative[String, String, String])
 checkAll("Rule[IorMessages, Int, String]", ProfunctorTests[Rule[IorMessages, ?, ?]].profunctor[Int, Int, Int, String, String, String])
}
