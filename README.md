# Checklist

Library for reading and validating data, with support for hard and soft constraints.
Pre-alpha. Not ready for use.

Copyright 2016-20 Dave Gurnell. Licensed [Apache 2][license].

[![Build Status](https://travis-ci.org/davegurnell/checklist.svg?branch=develop)](https://travis-ci.org/davegurnell/checklist)
[![Coverage status](https://img.shields.io/codecov/c/github/davegurnell/checklist/develop.svg)](https://codecov.io/github/davegurnell/checklist)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.davegurnell/checklist_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.davegurnell/checklist_2.12)

[license]: http://www.apache.org/licenses/LICENSE-2.0

## Getting It

Add one of the following to your `build.sbt`:

```scala
// For regular Scala projects:
libraryDependencies += "com.davegurnell" %% "checklist" % "<<VERSION>>"

// For ScalaJS projects:
libraryDependencies += "com.davegurnell" %%% "checklist" % "<<VERSION>>"

// Optional refinement module for Scala projects:
libraryDependencies += "com.davegurnell" %% "checklist-refinement" % "<<VERSION>>"

// Optional refinement module for ScalaJS projects:
libraryDependencies += "com.davegurnell" %%% "checklist-refinement" % "<<VERSION>>"
```

## Synopsis

Checklist is a library for validating data in applications and inputs to applications.
Key features include:

- hard and soft validation ("errors" and "warnings");
- accumulation of errors (as opposed to fail-fast error handling);
- the ability to transform values when validating them;
- recording the location of errors within an ADT using "Paths";
- a convenient shorthand syntax for tip-down validation of existing data.

The main concepts are as follows:

The main unit of code is a function-like `Rule` type.
A `Rule[A, B]` validates a value of type `A` and returns a value of type `B`.

Because validation can fail, the actual return type is `Checked[B]`,
which is a type alias for `Ior[NonEmptyList[Message], B]`.

A `Message` is a data structure containing
a `String` and a `Path` describing the location of the error.

### Hard vs Soft Validation

`Messages` come in two varieties:

- `ErrorMessages` represent "fatal" errors (hard validation);
- `WarningMessages` represent advisory messages (soft validation).

Here's an example of a hard validation rule:

```scala
import checklist._, Message.errors

val parseInt: Rule[String, Int] =
  Rule.pure { str =>
    Xor.catchNonFatal(str.toInt).fold(
      exn => Ior.left(errors("Must be an integer"))
      num => Ior.right(num)
    )
  }
```

and a soft rule:

```scala
import checklist._, .Message.warnings

def tooManyCoffees(recommendedLimit: Int): Rule[Int, Int] =
  Rule.pure { coffees =>
    if(coffees > recommendedLimit) {
      Ior.both(warnings("Hands shaking yet?"), coffees)
    } else {
      Ior.right(coffees)
    }
  }
```

### Built-in Rules

There are a host of built-in rules that do useful things
for example:

```scala
import checklist._, Rule._

val greaterThanZero: Rule[Int, Int] =
  gte(0)

val nonEmptyString: Rule[String, String] =
  nonEmpty[String]

def nonEmptyList[A]: Rule[List[A], List[A]] =
  nonEmpty[List[A]]
```

The built-in rules provide default English error messages
that make sense in a suitable context
(e.g. below a control on a web form).
However, you can specify your own messages for each:

```scala
import checklist._, Rule._

val greaterThanZero: Rule[Int, Int] =
  gte(0, Message.errors("Pull up!"))

val nonEmptyString: Rule[String, String] =
  nonEmpty[String](Message.errors("Talk to me!"))

def nonEmptyList[A]: Rule[List[A], List[A]] =
  nonEmpty[List[A]](Message.errors("Not enough data!"))
```

There are also a handful of useful constructor methods
for building your own rules:

```scala
import checklist._, Rule.test, Message.errors

val evenNumber: Rule[Int, Int] =
  test[Int](errors("That's odd...")) { num =>
    num % 2 == 0
  }
```

### Combining Rules

Checklist is built on top of Cats.
`Rule` has an instance of Cats' `Applicative` type class,
so we can combine them in parallel using Cartesian syntax:

```scala
import checklist._, Rule._
import cats.syntax.cartesian._

type Data = Map[String, String]

case class Coord(x: Int, y: Int)

val readCoord: Rule[Data, Coord] = (
  mapValue("x").andThen(parseInt).andThen(gte(0)) |@|
  mapValue("y").andThen(parseInt).andThen(gte(0))
).map(Coord.apply)
```

The error handling semantics are to gather as many errors as possible,
and construct a result value if possible.
The result is an `Ior` containing errors and/or the result value as appropriate:

```scala
readCoord(Map.empty)
// res0: checklist.Checked[Coord] = Left(
//   NonEmptyList(
//     ErrorMessage(Value not found,Path(x)),
//     ErrorMessage(Value not found,Path(y))
//   )
// )

readCoord(Map("x" -> "-1", "y" -> "-1"))
// res1: checklist.Checked[Coord] = Both(
//   NonEmptyList(
//     ErrorMessage(Must be greater than or equal to 0,Path()),
//     ErrorMessage(Must be greater than or equal to 0,Path())
//   ),
//   Coord(-1,-1)
// )

readCoord(Map("x" -> "0", "y" -> "0"))
// res2: checklist.Checked[Coord] = Right(Coord(0,0))
```

Checklist contains built-in functionality for calculating error `Paths`
from a variety of data types, including `Strings`, `Ints`, and other `Paths`:

```scala
import checklist._, Rule._
import cats.instances.list._ // for Traverse[List]

val readNestedLists: Rule[List[List[Int]], List[List[String]]] =
  gte(0).map(_ + "!").seq[List].seq[List]

readNestedLists(List(List(1, -2, 3), List(-4, 5, -6)))
// res3: checklist.Checked[List[List[String]]] = Both(
//   NonEmptyList(
//     ErrorMessage(Must be greater than or equal to 0,Path(0/1)),
//     ErrorMessage(Must be greater than or equal to 0,Path(1/0)),
//     ErrorMessage(Must be greater than or equal to 0,Path(1/2))
//   ),
//   List(List(1!, -2!, 3!), List(-4!, 5!, -6!))
// )
```

### Top-Down Syntax

Checklist uses macros and Monocle lenses to make it easy
to validate and clean existing data.
Here's an example:

```scala
import checklist._, Rule._

// The `field` macro below makes use of higher kinded types:
import scala.language.higherKinds

case class Address(house: Int, street: String)

implicit val checkAddress: Rule[Address, Address] =
  Rule[Address]
    .field(_.house)(gte(1))
    .field(_.street)(trimString andThen nonEmpty)
```

This rule picks up errors in input values:

```scala
checkAddress(Address(-1, ""))
// res4: checklist.Checked[Address] = Both(
//   NonEmptyList(
//     ErrorMessage(Must be greater than or equal to 1,Path(house)),
//     ErrorMessage(Must not be empty,Path(street))
//   ),
//   Address(-1,)
// )
```

and cleans data as it goes (note the trimmed street name):

```scala
checkAddress(Address(29, "   Acacia Road   "))
// res5: checklist.Checked[Address] = Right(Address(29,Acacia Road))
```

It can even pick up street names that are empty after trimming:

```scala
checkAddress(Address(29, "   "))
// res6: checklist.Checked[Address] = Both(
//   NonEmptyList(
//     ErrorMessage(Must not be empty,Path(street))
//   ),
//   Address(29,)
// )
```

### A Complete Example

Here's a more complete example involving nested case classes.
Note that the `Rule` for `Address` is picked up implicitly
when defining the `Rule` for `Person`:

```scala
import checklist._, Rule._
import scala.language.higherKinds

case class Address(house: Int, street: String)
case class Person(name: String, age: Int, address: Address)

implicit val checkAddress: Rule[Address, Address] =
  Rule[Address]
    .field(_.house)(gte(1))
    .field(_.street)(nonEmpty)

implicit val checkPerson: Rule[Person, Person] =
  Rule[Person]
    .field(_.name)(nonEmpty)
    .field(_.age)(gte(1))
    .field(_.address)
```

Also note that the paths in the error messages take into account
their absolute position within the data being validated:

```scala
checkAddress(Address(0, ""))
// res7: checklist.Checked[Address] = Both(
//   NonEmptyList(
//     ErrorMessage(Must be greater than or equal to 1,Path(house)),
//     ErrorMessage(Must not be empty,Path(street))
//   ),
//   Address(0,)
// )

checkPerson(Person("", 0, Address(0, "")))
// res8: checklist.Checked[Person] = Both(
//   NonEmptyList(
//     ErrorMessage(Must not be empty,Path(name)),
//     ErrorMessage(Must be greater than or equal to 1,Path(age)),
//     ErrorMessage(Must be greater than or equal to 1,Path(address/house)),
//     ErrorMessage(Must not be empty,Path(address/street))
//   ),
//   Person(,0,Address(0,))
// )

checkPerson(Person("Eric Wimp", 11, Address(29, "Acacia Road")))
// res9: checklist.Checked[Person] = Right(Person(Eric Wimp,11,Address(29,Acacia Road)))
~~~
