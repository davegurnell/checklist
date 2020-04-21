package checklist.laws

import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{ApplicativeTests, ProfunctorTests}
import cats.implicits._
import checklist._
import cats.laws.discipline.MiniInt

class RuleLawTests extends CatsSuite {
  checkAll("Rule[Int, String]", ApplicativeTests[Rule[MiniInt, ?]].applicative[String, String, String])
  checkAll("Rule[Int, String]", ProfunctorTests[Rule].profunctor[MiniInt, Int, Int, String, String, String])
}
