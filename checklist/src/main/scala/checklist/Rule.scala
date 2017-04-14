package checklist

import cats.{Applicative, Traverse}
import cats.data.Ior
import cats.syntax.functor._
import cats.syntax.traverse._
import monocle.{PLens, Lens}
import scala.language.experimental.macros
import scala.language.higherKinds
import scala.util.matching.Regex
import Message.{errors, warnings}

sealed abstract class Rule[A, B] {
  def apply(value: A): Checked[B]

  def map[C](func: B => C): Rule[A, C] =
    Rule.pure(value => this(value) map func)

  def contramap[C](func: C => A): Rule[C, B] =
    Rule.pure(value => this(func(value)))

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
            case Ior.Right(c)      => Ior.left(msg1)
            case Ior.Both(msg2, c) => Ior.left(msg1 concat msg2)
          }
        case Ior.Right(b) =>
          that(a) match {
            case Ior.Left(msg2)    => Ior.left(msg2)
            case Ior.Right(c)      => Ior.right((b, c))
            case Ior.Both(msg2, c) => Ior.both(msg2, (b, c))
          }
        case Ior.Both(msg1, b) =>
          that(a) match {
            case Ior.Left(msg2)    => Ior.left(msg1 concat msg2)
            case Ior.Right(c)      => Ior.both(msg1, (b, c))
            case Ior.Both(msg2, c) => Ior.both(msg1 concat msg2, (b, c))
          }
      }
    }

  def seq[S[_] : Indexable : Traverse]: Rule[S[A], S[B]] =
    Rule.pure { values =>
      val results: S[Checked[B]] =
        Indexable[S].zipWithIndex(values).map {
          case (value, index) =>
            this(value) leftMap (_ map (_ prefix index))
        }

      results.sequence
    }

  def opt: Rule[Option[A], Option[B]] =
    Rule.pure {
      case Some(value) => this(value) map (Some(_))
      case None        => Ior.right(None)
    }

  def req: Rule[Option[A], B] =
    req(errors("Value is required"))

  def req(messages: Messages): Rule[Option[A], B] =
    Rule.pure {
      case Some(value) => this(value)
      case None        => Ior.left(messages)
    }

  def prefix[P: PathPrefix](prefix: P): Rule[A, B] =
    Rule.pure(value => this(value) leftMap (_ map (_ prefix prefix)))

  def composeLens[S, T](lens: PLens[S, T, A, B]): Rule[S, T] =
    Rule.pure(value => this(lens.get(value)) map (lens.set(_)(value)))

  def at[P: PathPrefix, S, T](prefix: P, lens: PLens[S, T, A, B]): Rule[S, T] =
    this composeLens lens prefix prefix
}

object Rule extends BaseRules
  with ConverterRules
  with PropertyRules
  with RuleInstances
  with Rule1Syntax

trait BaseRules {
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

  def mapValue[A: PathPrefix, B](key: A): Rule[Map[A, B], B] =
    mapValue[A, B](key, errors(s"Value not found"))

  def mapValue[A: PathPrefix, B](key: A, messages: Messages): Rule[Map[A, B], B] =
    pure(map => map.get(key).map(Ior.right).getOrElse(Ior.left(messages map (_ prefix key))))

  val trimString: Rule[String, String] =
    pure(value => Ior.right(value.trim))
}

/** Rules that test a property of an existing value. */
trait PropertyRules {
  self: BaseRules =>

  def test[A](messages: => Messages)(func: A => Boolean): Rule[A, A] =
    pure(value => if(func(value)) Ior.right(value) else Ior.both(messages, value))

  def eql[A](comp: A): Rule[A, A] =
    eql(comp, errors(s"Must be ${comp}"))

  def eql[A](comp: A, messages: Messages): Rule[A, A] =
    test(messages)(_ == comp)

  def neq[A](comp: A): Rule[A, A] =
    neq[A](comp: A, errors(s"Must not be ${comp}"))

  def neq[A](comp: A, messages: Messages): Rule[A, A] =
    test(messages)(_ != comp)

  def gt[A](comp: A)(implicit ord: Ordering[A]): Rule[A, A] =
    gt(comp, errors(s"Must be greater than ${comp}"))

  def gt[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    test(messages)(ord.gt(_, comp))

  def lt[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    lt(comp, errors(s"Must be less than ${comp}"))

  def lt[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    test(messages)(ord.lt(_, comp))

  def gte[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    gte(comp, errors(s"Must be greater than or equal to ${comp}"))

  def gte[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    test(messages)(ord.gteq(_, comp))

  def lte[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    lte(comp, errors(s"Must be less than or equal to ${comp}"))

  def lte[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    test(messages)(ord.lteq(_, comp))

  def nonEmpty[S <% Seq[_]]: Rule[S, S] =
    nonEmpty(errors(s"Must not be empty"))

  def nonEmpty[S <% Seq[_]](messages: Messages): Rule[S, S] =
    test(messages)(value => (value : Seq[_]).nonEmpty)

  def lengthLt[S <% Seq[_]](comp: Int): Rule[S, S] =
    lengthLt(comp, errors(s"Must be length ${comp} or greater"))

  def lengthLt[S <% Seq[_]](comp: Int, messages: Messages): Rule[S, S] =
    test(messages)(value => (value : Seq[_]).length < comp)

  def lengthGt[S <% Seq[_]](comp: Int): Rule[S, S] =
    lengthGt(comp, errors(s"Must be length ${comp} or shorter"))

  def lengthGt[S <% Seq[_]](comp: Int, messages: Messages): Rule[S, S] =
    test(messages)(value => (value : Seq[_]).length > comp)

  def lengthLte[S <% Seq[_]](comp: Int): Rule[S, S] =
    lengthLte(comp, errors(s"Must be length ${comp} or greater"))

  def lengthLte[S <% Seq[_]](comp: Int, messages: Messages): Rule[S, S] =
    test(messages)(value => (value : Seq[_]).length <= comp)

  def lengthGte[S <% Seq[_]](comp: Int): Rule[S, S] =
    lengthGte(comp, errors(s"Must be length ${comp} or shorter"))

  def lengthGte[S <% Seq[_]](comp: Int, messages: Messages): Rule[S, S] =
    test(messages)(value => (value : Seq[_]).length >= comp)

  def matchesRegex(regex: Regex): Rule[String, String] =
    matchesRegex(regex, errors(s"Must match the pattern '${regex}'"))

  def matchesRegex(regex: Regex, messages: Messages): Rule[String, String] =
    test(messages)(regex.findFirstIn(_).isDefined)

  def containedIn[A](values: Seq[A]): Rule[A, A] =
    containedIn(values, errors(s"Must be one of the values ${values.mkString(", ")}"))

  def containedIn[A](values: Seq[A], messages: Messages): Rule[A, A] =
    test(messages)(value => values contains value)

  def notContainedIn[A](values: Seq[A]): Rule[A, A] =
    notContainedIn(values, errors(s"Must not be one of the values ${values.mkString(", ")}"))

  def notContainedIn[A](values: Seq[A], messages: Messages): Rule[A, A] =
    test(messages)(value => !(values contains value))
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

/* Per-field validation syntax for existing data */
trait Rule1Syntax {
  implicit class Rule1Ops[A](self: Rule1[A]) {
    def field[B](path: Path, lens: Lens[A, B])(implicit rule: Rule1[B]): Rule1[A] =
      self andThen rule.at(path, lens)

    def field[B](accessor: A => B)(implicit rule: Rule1[B]): Rule1[A] =
      macro RuleMacros.field[A, B]

    def fieldWith[B](path: Path, lens: Lens[A, B])(implicit builder: A => Rule1[B]): Rule1[A] =
      self andThen Rule.pure(value => builder(value).at(path, lens).apply(value))

    def fieldWith[B](accessor: A => B)(implicit builder: A => Rule1[B]): Rule1[A] =
      macro RuleMacros.fieldWith[A, B]
  }
}
