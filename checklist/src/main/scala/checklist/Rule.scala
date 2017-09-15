package checklist

import cats.{Applicative, Traverse, Monoid, Foldable}
import cats.data.Ior
import cats.implicits._
import monocle.PLens
import scala.language.higherKinds
import scala.util.matching.Regex
import Message.errors
import cats.data.NonEmptyList

sealed abstract class Rule[A, B] {
  def apply(value: A): Checked[B]

  def map[C](func: B => C): Rule[A, C] =
    Rule.pure(value => this(value) map func)

  def emap[C](func: B => Checked[C]): Rule[A, C] =
    Rule.pure(value => this(value) flatMap func)

  def recover(func: Messages => Checked[B]): Rule[A, B] =
    Rule.pure(value => this(value).fold(func, Ior.right, Ior.both))

  def mapMessages(func: Messages => Messages): Rule[A, B] =
    Rule.pure(value => this(value).fold(func andThen Ior.left, Ior.right, (msgs, r) => Ior.both(func(msgs), r)))

  def mapEachMessage(func: Message => Message): Rule[A, B] =
    mapMessages(_.map(func))

  def contramap[C](func: C => A): Rule[C, B] =
    Rule.pure(value => this(func(value)))

  def contramapPath[C, D: PathPrefix](path: D)(func: C => A): Rule[C, B] =
    contramap(func).mapEachMessage(_.prefix(path))

  def flatMap[C](func: B => Rule[A, C]): Rule[A, C] =
    Rule.pure(value => this(value) flatMap (func(_)(value)))

  def andThen[C](that: Rule[B, C]): Rule[A, C] =
    Rule.pure(value => this(value) flatMap (that.apply))

  def zip[C](that: Rule[A, C]): Rule[A, (B, C)] =
    Rule.pure { a =>
      this(a) match {
        case Ior.Left(msg1) =>
          that(a) match {
            case Ior.Left(msg2)    => Ior.left(msg1 concat msg2)
            case Ior.Both(msg2, _) => Ior.left(msg1 concat msg2)
            case Ior.Right(_)      => Ior.left(msg1)
          }
        case Ior.Both(msg1, b) =>
          that(a) match {
            case Ior.Left(msg2)    => Ior.left(msg1 concat msg2)
            case Ior.Both(msg2, c) => Ior.both(msg1 concat msg2, (b, c))
            case Ior.Right(c)      => Ior.both(msg1, (b, c))
          }
        case Ior.Right(b) =>
          that(a) match {
            case Ior.Left(msg2)    => Ior.left(msg2)
            case Ior.Both(msg2, c) => Ior.both(msg2, (b, c))
            case Ior.Right(c)      => Ior.right((b, c))
          }
      }
    }

  def seq[S[_]: Indexable: Traverse]: Rule[S[A], S[B]] =
    Rule.sequence(this)

  def opt: Rule[Option[A], Option[B]] =
    Rule.optional(this)

  def req: Rule[Option[A], B] =
    Rule.required(this)

  def prefix[P: PathPrefix](prefix: P): Rule[A, B] =
    mapEachMessage(_.prefix(prefix))

  def composeLens[S, T](lens: PLens[S, T, A, B]): Rule[S, T] =
    Rule.pure(value => this(lens.get(value)) map (lens.set(_)(value)))

  def at[P: PathPrefix, S, T](prefix: P, lens: PLens[S, T, A, B]): Rule[S, T] =
    this composeLens lens prefix prefix
}

object Rule extends BaseRules
  with ConverterRules
  with PropertyRules
  with CollectionRules
  with RuleInstances
  with Rule1Syntax

trait BaseRules {
  def apply[A]: Rule[A, A] =
    pure(Ior.right)

  def pure[A, B](func: A => Checked[B]): Rule[A, B] =
    new Rule[A, B] {
      def apply(value: A) =
        func(value)
    }

  def pass[A]: Rule[A, A] =
    pure(Ior.right)

  def fail[A](messages: Messages): Rule[A, A] =
    pure(Ior.both(messages, _))
}

/** Rules that convert one type to another. */
trait ConverterRules {
  self: BaseRules =>

  val parseInt: Rule[String, Int] =
    parseInt(errors("Must be a whole number"))

  def parseInt(messages: Messages): Rule[String, Int] =
    pure(value => util.Try(value.toInt).toOption.map(Ior.right).getOrElse(Ior.left(messages)))

  val parseDouble: Rule[String, Double] =
    parseDouble(errors("Must be a number"))

  def parseDouble(messages: Messages): Rule[String, Double] =
    pure(value => util.Try(value.toDouble).toOption.map(Ior.right).getOrElse(Ior.left(messages)))

  val trimString: Rule[String, String] =
    pure(value => Ior.right(value.trim))
}

/** Rules that test a property of an existing value. */
trait PropertyRules {
  self: BaseRules =>

  def test[A](messages: => Messages, strict: Boolean = false)(func: A => Boolean): Rule[A, A] =
    pure(value => if(func(value)) Ior.right(value) else {
      if(strict) Ior.left(messages)
      else Ior.both(messages, value)
    })

  def testStrict[A](messages: => Messages)(func: A => Boolean): Rule[A, A] =
    test(messages, true)(func)

  def eql[A](comp: A): Rule[A, A] =
    eql(comp, errors(s"Must be ${comp}"))

  def eql[A](comp: A, messages: Messages): Rule[A, A] =
    test(messages)(_ == comp)

  def eqlStrict[A](comp: A): Rule[A, A] =
    eql(comp, errors(s"Must be ${comp}"))

  def eqlStrict[A](comp: A, messages: Messages): Rule[A, A] =
    testStrict(messages)(_ == comp)

  def neq[A](comp: A): Rule[A, A] =
    neq[A](comp: A, errors(s"Must not be ${comp}"))

  def neq[A](comp: A, messages: Messages): Rule[A, A] =
    test(messages)(_ != comp)

  def neqStrict[A](comp: A): Rule[A, A] =
    neq[A](comp: A, errors(s"Must not be ${comp}"))

  def neqStrict[A](comp: A, messages: Messages): Rule[A, A] =
    testStrict(messages)(_ != comp)

  def gt[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    gt(comp, errors(s"Must be greater than ${comp}"))

  def gt[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    test(messages)(ord.gt(_, comp))

  def gtStrict[A](comp: A)(implicit ord: Ordering[A]): Rule[A, A] =
    gt(comp, errors(s"Must be greater than ${comp}"))

  def gtStrict[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    testStrict(messages)(ord.gt(_, comp))

  def lt[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    lt(comp, errors(s"Must be less than ${comp}"))

  def lt[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    test(messages)(ord.lt(_, comp))

  def ltStrict[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    lt(comp, errors(s"Must be less than ${comp}"))

  def ltStrict[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    testStrict(messages)(ord.lt(_, comp))

  def gte[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    gte(comp, errors(s"Must be greater than or equal to ${comp}"))

  def gte[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    test(messages)(ord.gteq(_, comp))

  def gteStrict[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    gte(comp, errors(s"Must be greater than or equal to ${comp}"))

  def gteStrict[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    testStrict(messages)(ord.gteq(_, comp))

  def lte[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    lte(comp, errors(s"Must be less than or equal to ${comp}"))

  def lte[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    test(messages)(ord.lteq(_, comp))

  def lteStrict[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    lte(comp, errors(s"Must be less than or equal to ${comp}"))

  def lteStrict[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    testStrict(messages)(ord.lteq(_, comp))

  def nonEmpty[S: Monoid]: Rule[S, S] =
    nonEmpty(errors(s"Must not be empty"))

  def nonEmpty[S: Monoid](messages: Messages): Rule[S, S] =
    test(messages)(value => value != Monoid[S].empty)

  def nonEmptyStrict[S: Monoid]: Rule[S, S] =
    nonEmpty(errors(s"Must not be empty"))

  def nonEmptyStrict[S: Monoid](messages: Messages): Rule[S, S] =
    testStrict(messages)(value => value != Monoid[S].empty)

  def lengthEq[F[_]: Foldable, A](comp: Int): Rule[F[A], F[A]] =
    lengthEq(comp, errors(s"Must be length ${comp} or greater"))

  def lengthEq[F[_]: Foldable, A](comp: Int, messages: Messages): Rule[F[A], F[A]] =
    test(messages)(_.size == comp)

  def lengthEqStrict[F[_]: Foldable, A](comp: Int): Rule[F[A], F[A]] =
    lengthEqStrict(comp, errors(s"Must be length ${comp} or greater"))

  def lengthEqStrict[F[_]: Foldable, A](comp: Int, messages: Messages): Rule[F[A], F[A]] =
    testStrict(messages)(_.size == comp)

  def lengthLt[F[_]: Foldable, A](comp: Int): Rule[F[A], F[A]] =
    lengthLt(comp, errors(s"Must be length ${comp} or greater"))

  def lengthLt[F[_]: Foldable, A](comp: Int, messages: Messages): Rule[F[A], F[A]] =
    test(messages)(_.size < comp)

  def lengthLtStrict[F[_]: Foldable, A](comp: Int): Rule[F[A], F[A]] =
    lengthLt(comp, errors(s"Must be length ${comp} or greater"))

  def lengthLtStrict[F[_]: Foldable, A](comp: Int, messages: Messages): Rule[F[A], F[A]] =
    testStrict(messages)(_.size < comp)

  def lengthGt[F[_]: Foldable, A](comp: Int): Rule[F[A], F[A]] =
    lengthGt(comp, errors(s"Must be length ${comp} or shorter"))

  def lengthGt[F[_]: Foldable, A](comp: Int, messages: Messages): Rule[F[A], F[A]] =
    test(messages)(_.size > comp)

  def lengthGtStrict[F[_]: Foldable, A](comp: Int): Rule[F[A], F[A]] =
    lengthGt(comp, errors(s"Must be length ${comp} or shorter"))

  def lengthGtStrict[F[_]: Foldable, A](comp: Int, messages: Messages): Rule[F[A], F[A]] =
    testStrict(messages)(_.size > comp)

  def lengthLte[F[_]: Foldable, A](comp: Int): Rule[F[A], F[A]] =
    lengthLte(comp, errors(s"Must be length ${comp} or greater"))

  def lengthLte[F[_]: Foldable, A](comp: Int, messages: Messages): Rule[F[A], F[A]] =
    test(messages)(_.size <= comp)

  def lengthLteStrict[F[_]: Foldable, A](comp: Int): Rule[F[A], F[A]] =
    lengthLte(comp, errors(s"Must be length ${comp} or greater"))

  def lengthLteStrict[F[_]: Foldable, A](comp: Int, messages: Messages): Rule[F[A], F[A]] =
    testStrict(messages)(_.size <= comp)

  def lengthGte[F[_]: Foldable, A](comp: Int): Rule[F[A], F[A]] =
    lengthGte(comp, errors(s"Must be length ${comp} or shorter"))

  def lengthGte[F[_]: Foldable, A](comp: Int, messages: Messages): Rule[F[A], F[A]] =
    test(messages)(_.size >= comp)

  def lengthGteStrict[F[_]: Foldable, A](comp: Int): Rule[F[A], F[A]] =
    lengthGte(comp, errors(s"Must be length ${comp} or shorter"))

  def lengthGteStrict[F[_]: Foldable, A](comp: Int, messages: Messages): Rule[F[A], F[A]] =
    testStrict(messages)(_.size >= comp)

  def nonEmptyList[A]: Rule[List[A], NonEmptyList[A]] =
    nonEmptyList(errors("Must not be empty"))

  def nonEmptyList[A](messages: Messages): Rule[List[A], NonEmptyList[A]] =
    Rule.pure {
      case Nil => Ior.left(messages)
      case h :: t => Ior.right(NonEmptyList(h, t))
    }

  def matchesRegex(regex: Regex): Rule[String, String] =
    matchesRegex(regex, errors(s"Must match the pattern '${regex}'"))

  def matchesRegex(regex: Regex, messages: Messages): Rule[String, String] =
    test(messages)(regex.findFirstIn(_).isDefined)

  def matchesRegexStrict(regex: Regex): Rule[String, String] =
    matchesRegex(regex, errors(s"Must match the pattern '${regex}'"))

  def matchesRegexStrict(regex: Regex, messages: Messages): Rule[String, String] =
    testStrict(messages)(regex.findFirstIn(_).isDefined)

  def containedIn[A](values: Seq[A]): Rule[A, A] =
    containedIn(values, errors(s"Must be one of the values ${values.mkString(", ")}"))

  def containedIn[A](values: Seq[A], messages: Messages): Rule[A, A] =
    test(messages)(value => values contains value)

  def containedInStrict[A](values: Seq[A]): Rule[A, A] =
    containedIn(values, errors(s"Must be one of the values ${values.mkString(", ")}"))

  def containedInStrict[A](values: Seq[A], messages: Messages): Rule[A, A] =
    testStrict(messages)(value => values contains value)

  def notContainedIn[A](values: Seq[A]): Rule[A, A] =
    notContainedIn(values, errors(s"Must not be one of the values ${values.mkString(", ")}"))

  def notContainedIn[A](values: Seq[A], messages: Messages): Rule[A, A] =
    test(messages)(value => !(values contains value))

  def notContainedInStrict[A](values: Seq[A]): Rule[A, A] =
    notContainedIn(values, errors(s"Must not be one of the values ${values.mkString(", ")}"))

  def notContainedInStrict[A](values: Seq[A], messages: Messages): Rule[A, A] =
    testStrict(messages)(value => !(values contains value))
}

trait CollectionRules {
  self: BaseRules =>

  def optional[A, B](rule: Rule[A, B]): Rule[Option[A], Option[B]] =
    pure {
      case Some(value) => rule(value) map (Some(_))
      case None        => Ior.right(None)
    }

  def required[A, B](rule: Rule[A, B]): Rule[Option[A], B] =
    required(rule, errors("Value is required"))

  def required[A, B](rule: Rule[A, B], messages: Messages): Rule[Option[A], B] =
    pure {
      case Some(value) => rule(value)
      case None        => Ior.left(messages)
    }

  def sequence[S[_] : Indexable : Traverse, A, B](rule: Rule[A, B]): Rule[S[A], S[B]] =
    pure { values =>
      Indexable[S].zipWithIndex(values).traverse {
        case (value, index) =>
          rule.prefix(index).apply(value)
      }
    }

  def mapValue[A: PathPrefix, B](key: A): Rule[Map[A, B], B] =
    mapValue[A, B](key, errors(s"Value not found"))

  def mapValue[A: PathPrefix, B](key: A, messages: Messages): Rule[Map[A, B], B] =
    pure(map => map.get(key).map(Ior.right).getOrElse(Ior.left(messages map (_ prefix key))))

  def mapValues[A: PathPrefix, B, C](rule: Rule[B, C]): Rule[Map[A, B], Map[A, C]] =
    pure { in: Map[A, B] =>
      in.toList.traverse {
        case (key, value) =>
          rule.prefix(key).apply(value).map(key -> _)
      }
    } map (_.toMap)
}

/** Type class instances for Rule */
trait RuleInstances {
  self: BaseRules =>

  implicit def ruleApplicative[A]: Applicative[Rule[A, ?]] =
    new Applicative[Rule[A, ?]] {
      def pure[B](value: B): Rule[A, B] =
        Rule.pure(_ => Ior.right(value))

      def ap[B, C](funcRule: Rule[A, B => C])(argRule: Rule[A, B]): Rule[A, C] =
        (funcRule zip argRule) map { pair =>
          val (func, arg) = pair
          func(arg)
        }

      override def map[B, C](rule: Rule[A, B])(func: B => C): Rule[A, C] =
        rule map func

      override def product[B, C](rule1: Rule[A, B], rule2: Rule[A, C]): Rule[A, (B, C)] =
        rule1 zip rule2
    }
}
