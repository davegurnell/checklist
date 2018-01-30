package checklist.laws

import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{ApplicativeTests, ProfunctorTests}
import cats.tests.CatsSuite
import checklist._

class RuleLawTests extends CatsSuite {
  checkAll("Rule[Int, String]", ApplicativeTests[Rule[Int, ?]].applicative[String, String, String])
  checkAll("Rule[Int, String]", ProfunctorTests[Rule].profunctor[Int, Int, Int, String, String, String])
}
