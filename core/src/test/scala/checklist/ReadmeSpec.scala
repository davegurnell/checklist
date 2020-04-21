package checklist

import cats.data.Ior
import cats.implicits._
import org.scalatest._
import org.scalatest.freespec._
import org.scalatest.matchers.should._

class ReadmeSpec extends AnyFreeSpec with Matchers {
  import Rule._
  import Message._

  case class Address(house: Int, street: String)
  case class Person(name: String, age: Int, address: Address)
  case class Business(name: String, addresses: List[Address])

  implicit val addressRule: Rule1[Address] =
    Rule
      .pass[Address]
      .field(_.house)(gte(1))
      .field(_.street)(nonEmpty[String])
      .fieldWith(_.house) { address =>
        address.street match {
          case "Acacia Road" =>
            lte(29, warnings("There are only 29 houses on Acacia Road"))
          case _ => pass
        }
      }

  implicit val personRule: Rule1[Person] =
    Rule
      .pass[Person]
      .field(_.name)(nonEmpty[String])
      .field(_.age)(gte(1))
      .field(_.address)

  implicit val businessRule: Rule1[Business] =
    Rule
      .pass[Business]
      .field(_.name)(nonEmpty[String])
      .field(_.addresses)(sequence(addressRule))

  "example from the readme" - {
    "should validate a valid person" in {
      val bananaman = Person("Eric Wimp", 11, Address(29, "Acacia Road"))
      personRule(bananaman) should be(Ior.right(bananaman))
    }

    "should validate an invalid person" in {
      val invalid = Person("", 0, Address(0, ""))
      personRule(invalid) should be(
        Ior.both(
          errors(
            ("name" :: PNil) -> "Must not be empty",
            ("age" :: PNil) -> "Must be greater than or equal to 1",
            ("address" :: "house" :: PNil) -> "Must be greater than or equal to 1",
            ("address" :: "street" :: PNil) -> "Must not be empty"
          ),
          invalid
        )
      )
    }

    "should correct factual errors about bananaman" in {
      val bananaman = Person("Eric Wimp", 11, Address(30, "Acacia Road"))
      personRule(bananaman) should be(
        Ior.both(
          warnings(
            ("address" :: "house" :: PNil) -> "There are only 29 houses on Acacia Road"
          ),
          bananaman
        )
      )
    }
  }
}
