package checklist
package std

import cats.data.Ior
import cats.instances.list._
import cats.instances.string._
import checklist.std.ior._
import checklist.std.ior.syntax._
import org.scalatest._

class ReadmeSpec extends FreeSpec with Matchers {
  import Message._

  case class Address(house: Int, street: String)
  case class Person(name: String, age: Int, address: Address)
  case class Business(name: String, addresses: List[Address])

  implicit val addressRule: Rule1[IorMessages, Address] =
    rules.pass[Address]
      .field(_.house)(rules.gte(1))
      .field(_.street)(rules.nonEmpty[String])
      .fieldWith(_.house) { address =>
        address.street match {
          case "Acacia Road" => rules.lte(29, rules.warn("There are only 29 houses on Acacia Road"))
          case _             => rules.pass
        }
      }

  implicit val personRule: Rule1[IorMessages, Person] =
    rules.pass[Person]
      .field(_.name)(rules.nonEmpty[String])
      .field(_.age)(rules.gte(1))
      .field(_.address)

  implicit val businessRule: Rule1[IorMessages, Business] =
    rules.pass[Business]
      .field(_.name)(rules.nonEmpty[String])
      .field(_.addresses)(rules.sequence(addressRule))

  "example from the readme" - {
    "should validate a valid person" in {
      val bananaman: Person =
        Person("Eric Wimp", 11, Address(29, "Acacia Road"))

      personRule(bananaman) should be(Ior.right(bananaman))
    }

    "should validate an invalid person" in {
      val invalid: Person =
        Person("", 0, Address(0, ""))

      personRule(invalid) should be(Ior.both(
        errors(
          ("name"                :: PNil) -> "Must not be empty",
          ("age"                 :: PNil) -> "Must be greater than or equal to 1",
          ("address" :: "house"  :: PNil) -> "Must be greater than or equal to 1",
          ("address" :: "street" :: PNil) -> "Must not be empty"
        ),
        invalid
      ))
    }

    "should correct factual errors about bananaman" in {
      val bananaman: Person =
        Person("Eric Wimp", 11, Address(30, "Acacia Road"))

      personRule(bananaman) should be(Ior.both(
        warnings(
          ("address" :: "house" :: PNil) -> "There are only 29 houses on Acacia Road"
        ),
        bananaman
      ))
    }
  }
}
