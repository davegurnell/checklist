package checklist.laws

import cats.implicits.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.{ApplicativeTests, MiniInt, ProfunctorTests}
import checklist.*

class RuleLawTests extends CatsSuite {
  checkAll("Rule[Int, String]", ApplicativeTests[Rule[MiniInt, *]].applicative[String, String, String])
  checkAll("Rule[Int, String]", ProfunctorTests[Rule].profunctor[MiniInt, Int, Int, String, String, String])
}
