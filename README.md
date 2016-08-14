# Checklist

Library for reading and validating data, with support for hard and soft constraints.
Pre-alpha. Not ready for use.

Copyright 2016 Dave Gurnell. Licensed [Apache 2][license].

## Synopsis

### Validating Existing Data

Checklist uses Monocle, Cats, and Scala macros
to provide a concise DSL for validating exsting data:

~~~ scala
import checklist._

case class Address(house: Int, street: String)
case class Person(name: String, age: Int, address: Address)

implicit val checkAddress: Rule1[Address] =
  Rule.pass[Address]
    .field(_.house)(warn.gte(1))
    .field(_.street)(warn.nonEmpty)

implicit val checkPerson: Rule1[Person] =
  Rule.pass[Person]
    .field(_.name)(nonEmpty)
    .field(_.age)(gte(1))
    .field(_.address)

checkPerson(Person("", 0, Address(0, "")))
~~~

### Validating and Reading Dynamically Typed Data

TODO: Complete

### Validating Updates to Data

TODO: Complete

[license]: http://www.apache.org/licenses/LICENSE-2.0
