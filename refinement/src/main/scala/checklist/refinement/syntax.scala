package checklist.refinement

/**
 * Provides rule-builder style methods for refining types during the validation process.
 *
 * This can be useful for situations where your input types may be raw user input data which you validate, and then from that
 * validation you gain additional context.
 *
 * Consider The example below:
 * {{{
 * case class InputFoo(untrimmed: String, maybeEmptyList: List[Int], name: String)
 * case class ValidatedFoo(trimmed: String, nonEmptyList: NonEmptyList[Int], name: String, thing: Double)
 *
 * val rule =
 *   Rule.builder[InputFoo]
 *     .check("untrimmed", _.untrimmed)(Rule.trimString)
 *     .check("maybeEmptyList", _.maybeEmptyList)(Rule.nonEmptyList)
 *     .ignore(_.name) // No validation will be performed, but the value for name will be passed through.
 *     .append(util.Random.nextDouble()) // Appends a random double to the output type
 *     .build[ValidatedFoo]
 *
 * rule(InputFoo("bar ", List("baz"))) // Ior.Right(ValidatedFoo("bar", NonEmptyList("baz")))
 * rule(InputFoo("bar", List()) // Ior.Left(NonEmptyList(ErrorMessage("Must not be empty", PField("maybeEmptyList", PNil))))
 * }}}
 *
 * A user has input an untrimmed string, and a list that may be empty.   During the sanitization and validation process, the
 * string is trimmed, and then the List[Int] becomes a NonEmptyList[Int]. This is because the sanitization process can refine the
 * type and hold on to context from the validation phase. This could also be used in conjunction with things like string parsing
 * engines to accept user input strings, parse them in to your AST during sanitization/validation.
 *
 */
object syntax extends RuleHListSyntax
