package checklist

import cats.{Applicative, Id, Monad, Monoid, Traverse, ~>}
import cats.data.{Ior, IorT}
import cats.implicits._
import monocle.PLens

import scala.language.higherKinds
import scala.util.matching.Regex
import Message.errors
//import cats.arrow.Profunctor
import cats.data.NonEmptyList
import checklist.SizeableSyntax._

/**
  * A Rule validates/sanitizes a value of type `A` producing a type `B` inside an `Ior[NonEmptyList[Messages], B]`
  *
  * @tparam A The type to be validated
  * @tparam B The type to be produced
  */
sealed abstract class Rule[F[_] : Applicative, A, B] {
  /**
    * Performs validation on `A`
    *
    * @tparam A The value to be validated
    */
  def apply(value: A): F[Checked[B]]

  def map[C](func: B => C): Rule[F, A, C] =
    Rule.pure(value => this (value).map(_.map(func)))

  /**
    * Maps the result type with the potential for a failure to occur.
    */
  def emap[C](func: B => Checked[C]): Rule[F, A, C] =
    Rule.pure(value => this (value).map(_.flatMap(func)))

  def recover(func: Messages => Checked[B]): Rule[F, A, B] =
    Rule.pure(value => this (value).map(_.fold(func, Ior.right, Ior.both)))

  def mapMessages(func: Messages => Messages): Rule[F, A, B] =
    Rule.pure(value => this (value).map(_.fold(func andThen Ior.left, Ior.right, (msgs, r) => Ior.both(func(msgs), r))))

  def mapEachMessage(func: Message => Message): Rule[F, A, B] =
    mapMessages(_.map(func))

  def contramap[C](func: C => A): Rule[F, C, B] =
    Rule.pure(value => this (func(value)))

  def contramapPath[C, D: PathPrefix](path: D)(func: C => A): Rule[F, C, B] =
    contramap(func).mapEachMessage(_.prefix(path))

  def flatMap[C](func: B => Rule[F, A, C])(implicit ev: Monad[F]): Rule[F, A, C] =
    Rule.pure[F, A, C] { a =>
      (for {
        b <- IorT(this (a))
        c <- IorT(func(b)(a))
      } yield {
        c
      }).value
    }

  def liftWith[G[_] : Applicative](transform: F ~> G): Rule[G, A, B] = Rule.pure[G, A, B] { in => this (in).liftTo[G](transform) }

  def liftTo[G[_] : Applicative](implicit transform: F ~> G): Rule[G, A, B] = liftWith(transform)

  def andThen[G[_] : Applicative, R[_], C](that: Rule[G, B, C])(implicit canLift: CanLift[Monad, F, G, R]): Rule[R, A, C] = {
    implicit val evMonadR: Monad[R] = canLift.evR

    Rule.pure[R, A, C](a =>
      (for {
        b <- IorT(this.liftWith(canLift.liftF).apply(a))
        c <- IorT(that.liftWith(canLift.liftG).apply(b))
      } yield {
        c
      }).value
    )
  }

  def zip[G[_] : Applicative, R[_], C](that: Rule[G, A, C])(implicit canLift: CanLift[Applicative, F, G, R]): Rule[R, A, (B, C)] = {
    implicit val evApplicativeR: Applicative[R] = canLift.evR

    Rule.pure { a =>
      (this.liftWith(canLift.liftF).apply(a), that.liftWith(canLift.liftG).apply(a)).mapN((checkedB, checkedC) =>
        checkedB match {
          case Ior.Left(msg1) =>
            checkedC match {
              case Ior.Left(msg2) => Ior.left(msg1 concatNel msg2)
              case Ior.Both(msg2, _) => Ior.left(msg1 concatNel msg2)
              case Ior.Right(_) => Ior.left(msg1)
            }
          case Ior.Both(msg1, b) =>
            checkedC match {
              case Ior.Left(msg2) => Ior.left(msg1 concatNel msg2)
              case Ior.Both(msg2, c) => Ior.both(msg1 concatNel msg2, (b, c))
              case Ior.Right(c) => Ior.both(msg1, (b, c))
            }
          case Ior.Right(b) =>
            checkedC match {
              case Ior.Left(msg2) => Ior.left(msg2)
              case Ior.Both(msg2, c) => Ior.both(msg2, (b, c))
              case Ior.Right(c) => Ior.right((b, c))
            }
        }
      )
    }
  }

  def seq[S[_] : Traverse]: Rule[F, S[A], S[B]] =
    Rule.sequence(this)

  def opt: Rule[F, Option[A], Option[B]] =
    Rule.optional(this)

  def req: Rule[F, Option[A], B] =
    Rule.required(this)

  def prefix[P: PathPrefix](prefix: P): Rule[F, A, B] =
    mapEachMessage(_.prefix(prefix))

  def composeLens[S, T](lens: PLens[S, T, A, B]): Rule[F, S, T] =
    Rule.pure(value => this (lens.get(value)).map(_ map (lens.set(_)(value))))

  def at[P: PathPrefix, S, T](prefix: P, lens: PLens[S, T, A, B]): Rule[F, S, T] =
    this composeLens lens prefix prefix

  // Need to think this more thoroughly
  //  def kleisli: Kleisli[Checked, A, B] = Kleisli(apply)
}

/*
// A Check isn't just a Rule that returns the same type, but it actually returns the same value. A "pure validator".
// Should we take this approach, we have to reprogram most of Rule's methods to return Check whenever they are combined with another Check.
sealed abstract class Check[F[_] : Applicative, A] extends Rule[F, A, A] {
  def validate(value: A): F[Checked[Unit]]

  override final def apply(value: A) = validate(value).map(_.map(_ => value))

  override def liftWith[G[_] : Applicative](transform: F ~> G): Check[G, A] = Check.pure[G, A] { in => this (in).liftTo[G](transform) }

  override def liftTo[G[_] : Applicative](implicit transform: F ~> G): Check[G, A] = liftWith(transform)
}

object Check {
  def pure[F[_] : Applicative, A](func: A => F[Checked[Unit]]): Check[F, A] =
    new Check[F, A] {
      def validate(value: A) =
        func(value)
    }
}
*/

object Rule extends BaseRules
  with ConverterRules
  with PropertyRules
  with CollectionRules
  with RuleInstances
  with CheckSyntax

trait BaseRules {
  def apply[F[_] : Applicative, A]: Check[F, A] =
    Rule.pure(in => Applicative[F].pure(Ior.right(in)))

  def pure[F[_] : Applicative, A, B](func: A => F[Checked[B]]): Rule[F, A, B] =
    new Rule[F, A, B] {
      def apply(value: A) =
        func(value)
    }

  //  def fromKleisli[A, B](func: Kleisli[Checked, A, B]): Rule[A, B] = pure(func.apply)

  def pass[A]: Check[Id, A] =
    apply[Id, A]

  def fail[A](messages: Messages): Check[Id, A] =
    Rule.pure[Id, A, A](in => Ior.both(messages, in))
}

/** Rules that convert one type to another. */
trait ConverterRules {
  self: BaseRules =>

  val parseInt: Rule[Id, String, Int] =
    parseInt(errors("Must be a whole number"))

  def parseInt(messages: Messages): Rule[Id, String, Int] =
    pure[Id, String, Int](value => util.Try(value.toInt).toOption.map(Ior.right).getOrElse(Ior.left(messages)))

  val parseDouble: Rule[Id, String, Double] =
    parseDouble(errors("Must be a number"))

  def parseDouble(messages: Messages): Rule[Id, String, Double] =
    pure[Id, String, Double](value => util.Try(value.toDouble).toOption.map(Ior.right).getOrElse(Ior.left(messages)))

  val trimString: Rule[Id, String, String] =
    pure[Id, String, String](value => Ior.right(value.trim))
}

/** Rules that test a property of an existing value. */
trait PropertyRules {
  self: BaseRules =>

  def test[F[_] : Applicative, A](messages: => Messages, strict: Boolean = false)(func: A => F[Boolean]): Check[F, A] =
    Rule.pure(value => func(value).map { result =>
      if (result) Ior.right(value) else {
        if (strict) Ior.left(messages)
        else Ior.both(messages, value)
      }
    })

  def testStrict[F[_] : Applicative, A](messages: => Messages)(func: A => F[Boolean]): Check[F, A] =
    test(messages, strict = true)(func)

  def eql[A](comp: A): Check[Id, A] =
    eql(comp, errors(s"Must be ${comp}"))

  def eql[A](comp: A, messages: Messages): Check[Id, A] =
    test[Id, A](messages)(_ == comp)

  def eqlStrict[A](comp: A): Check[Id, A] =
    eqlStrict(comp, errors(s"Must be ${comp}"))

  def eqlStrict[A](comp: A, messages: Messages): Check[Id, A] =
    testStrict[Id, A](messages)(_ == comp)

  def neq[A](comp: A): Check[Id, A] =
    neq[A](comp: A, errors(s"Must not be ${comp}"))

  def neq[A](comp: A, messages: Messages): Check[Id, A] =
    test[Id, A](messages)(_ != comp)

  def neqStrict[A](comp: A): Check[Id, A] =
    neqStrict[A](comp: A, errors(s"Must not be ${comp}"))

  def neqStrict[A](comp: A, messages: Messages): Check[Id, A] =
    testStrict[Id, A](messages)(_ != comp)

  def gt[A](comp: A)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    gt(comp, errors(s"Must be greater than ${comp}"))

  def gt[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    test[Id, A](messages)(ord.gt(_, comp))

  def gtStrict[A](comp: A)(implicit ord: Ordering[A]): Check[Id, A] =
    gtStrict(comp, errors(s"Must be greater than ${comp}"))

  def gtStrict[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    testStrict[Id, A](messages)(ord.gt(_, comp))

  def lt[A](comp: A)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    lt(comp, errors(s"Must be less than ${comp}"))

  def lt[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    test[Id, A](messages)(ord.lt(_, comp))

  def ltStrict[A](comp: A)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    ltStrict(comp, errors(s"Must be less than ${comp}"))

  def ltStrict[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    testStrict[Id, A](messages)(ord.lt(_, comp))

  def gte[A](comp: A)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    gte(comp, errors(s"Must be greater than or equal to ${comp}"))

  def gte[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    test[Id, A](messages)(ord.gteq(_, comp))

  def gteStrict[A](comp: A)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    gteStrict(comp, errors(s"Must be greater than or equal to ${comp}"))

  def gteStrict[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    testStrict[Id, A](messages)(ord.gteq(_, comp))

  def lte[A](comp: A)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    lte(comp, errors(s"Must be less than or equal to ${comp}"))

  def lte[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    test[Id, A](messages)(ord.lteq(_, comp))

  def lteStrict[A](comp: A)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    lteStrict(comp, errors(s"Must be less than or equal to ${comp}"))

  def lteStrict[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Check[Id, A] =
    testStrict[Id, A](messages)(ord.lteq(_, comp))

  def nonEmpty[S: Monoid]: Check[Id, S] =
    nonEmpty(errors(s"Must not be empty"))

  def nonEmpty[S: Monoid](messages: Messages): Check[Id, S] =
    test[Id, S](messages)(value => value != Monoid[S].empty)

  def nonEmptyStrict[S: Monoid]: Check[Id, S] =
    nonEmptyStrict(errors(s"Must not be empty"))

  def nonEmptyStrict[S: Monoid](messages: Messages): Check[Id, S] =
    testStrict[Id, S](messages)(value => value != Monoid[S].empty)

  def lengthEq[A: Sizeable](comp: Int): Check[Id, A] =
    lengthEq(comp, errors(s"Must be length ${comp}"))

  def lengthEq[A: Sizeable](comp: Int, messages: Messages): Check[Id, A] =
    test[Id, A](messages)(_.size == comp)

  def lengthEqStrict[A: Sizeable](comp: Int): Check[Id, A] =
    lengthEqStrict(comp, errors(s"Must be length ${comp}"))

  def lengthEqStrict[A: Sizeable](comp: Int, messages: Messages): Check[Id, A] =
    testStrict[Id, A](messages)(_.size == comp)

  def lengthLt[A: Sizeable](comp: Int): Check[Id, A] =
    lengthLt(comp, errors(s"Must be shorter than length ${comp}"))

  def lengthLt[A: Sizeable](comp: Int, messages: Messages): Check[Id, A] =
    test[Id, A](messages)(_.size < comp)

  def lengthLtStrict[A: Sizeable](comp: Int): Check[Id, A] =
    lengthLtStrict(comp, errors(s"Must be shorter than length ${comp}"))

  def lengthLtStrict[A: Sizeable](comp: Int, messages: Messages): Check[Id, A] =
    testStrict[Id, A](messages)(_.size < comp)

  def lengthGt[A: Sizeable](comp: Int): Check[Id, A] =
    lengthGt(comp, errors(s"Must be longer than length ${comp}"))

  def lengthGt[A: Sizeable](comp: Int, messages: Messages): Check[Id, A] =
    test[Id, A](messages)(_.size > comp)

  def lengthGtStrict[A: Sizeable](comp: Int): Check[Id, A] =
    lengthGtStrict(comp, errors(s"Must be longer than length ${comp}"))

  def lengthGtStrict[A: Sizeable](comp: Int, messages: Messages): Check[Id, A] =
    testStrict[Id, A](messages)(_.size > comp)

  def lengthLte[A: Sizeable](comp: Int): Check[Id, A] =
    lengthLte(comp, errors(s"Must be length ${comp} or shorter"))

  def lengthLte[A: Sizeable](comp: Int, messages: Messages): Check[Id, A] =
    test[Id, A](messages)(_.size <= comp)

  def lengthLteStrict[A: Sizeable](comp: Int): Check[Id, A] =
    lengthLteStrict(comp, errors(s"Must be length ${comp} or shorter"))

  def lengthLteStrict[A: Sizeable](comp: Int, messages: Messages): Check[Id, A] =
    testStrict[Id, A](messages)(_.size <= comp)

  def lengthGte[A: Sizeable](comp: Int): Check[Id, A] =
    lengthGte(comp, errors(s"Must be length ${comp} or longer"))

  def lengthGte[A: Sizeable](comp: Int, messages: Messages): Check[Id, A] =
    test[Id, A](messages)(_.size >= comp)

  def lengthGteStrict[A: Sizeable](comp: Int): Check[Id, A] =
    lengthGteStrict(comp, errors(s"Must be length ${comp} or longer"))

  def lengthGteStrict[A: Sizeable](comp: Int, messages: Messages): Check[Id, A] =
    testStrict[Id, A](messages)(_.size >= comp)

  def nonEmptyList[A]: Rule[Id, List[A], NonEmptyList[A]] =
    nonEmptyList(errors("Must not be empty"))

  def nonEmptyList[A](messages: Messages): Rule[Id, List[A], NonEmptyList[A]] =
    Rule.pure[Id, List[A], NonEmptyList[A]] {
      case Nil => Ior.left(messages)
      case h :: t => Ior.right(NonEmptyList(h, t))
    }

  def matchesRegex(regex: Regex): Check[Id, String] =
    matchesRegex(regex, errors(s"Must match the pattern '${regex}'"))

  def matchesRegex(regex: Regex, messages: Messages): Check[Id, String] =
    test[Id, String](messages)(regex.findFirstIn(_).isDefined)

  def matchesRegexStrict(regex: Regex): Check[Id, String] =
    matchesRegexStrict(regex, errors(s"Must match the pattern '${regex}'"))

  def matchesRegexStrict(regex: Regex, messages: Messages): Check[Id, String] =
    testStrict[Id, String](messages)(regex.findFirstIn(_).isDefined)

  def containedIn[A](values: Seq[A]): Rule[Id, A, A] =
    containedIn(values, errors(s"Must be one of the values ${values.mkString(", ")}"))

  def containedIn[A](values: Seq[A], messages: Messages): Check[Id, A] =
    test[Id, A](messages)(value => values contains value)

  def containedInStrict[A](values: Seq[A]): Check[Id, A] =
    containedInStrict(values, errors(s"Must be one of the values ${values.mkString(", ")}"))

  def containedInStrict[A](values: Seq[A], messages: Messages): Check[Id, A] =
    testStrict[Id, A](messages)(value => values contains value)

  def notContainedIn[A](values: Seq[A]): Check[Id, A] =
    notContainedIn(values, errors(s"Must not be one of the values ${values.mkString(", ")}"))

  def notContainedIn[A](values: Seq[A], messages: Messages): Check[Id, A] =
    test[Id, A](messages)(value => !(values contains value))

  def notContainedInStrict[A](values: Seq[A]): Check[Id, A] =
    notContainedInStrict(values, errors(s"Must not be one of the values ${values.mkString(", ")}"))

  def notContainedInStrict[A](values: Seq[A], messages: Messages): Check[Id, A] =
    testStrict[Id, A](messages)(value => !(values contains value))
}

trait CollectionRules {
  self: BaseRules =>

  def optional[F[_] : Applicative, A, B](rule: Rule[F, A, B]): Rule[F, Option[A], Option[B]] =
    pure {
      case Some(value) => rule(value).map(_ map (Some(_)))
      case None => Applicative[F].pure(Ior.right(None))
    }

  def required[F[_] : Applicative, A, B](rule: Rule[F, A, B]): Rule[F, Option[A], B] =
    required(rule, errors("Value is required"))

  def required[F[_] : Applicative, A, B](rule: Rule[F, A, B], messages: Messages): Rule[F, Option[A], B] =
    pure {
      case Some(value) => rule(value)
      case None => Applicative[F].pure(Ior.left(messages))
    }

  def sequence[F[_] : Applicative, S[_] : Traverse, A, B](rule: Rule[F, A, B]): Rule[F, S[A], S[B]] =
    pure { values =>
      values.zipWithIndex.traverse { case (value, index) =>
        rule.prefix(index).apply(value)
      }.map(_.sequence)
    }

  def mapValue[F[_] : Applicative, A: PathPrefix, B](key: A): Rule[F, Map[A, B], B] =
    mapValue[F, A, B](key, errors(s"Value not found"))

  def mapValue[F[_] : Applicative, A: PathPrefix, B](key: A, messages: Messages): Rule[F, Map[A, B], B] =
    pure(map => Applicative[F].pure(map.get(key).map(Ior.right).getOrElse(Ior.left(messages map (_ prefix key)))))

  def mapValues[F[_] : Applicative, A: PathPrefix, B, C](rule: Rule[F, B, C]): Rule[F, Map[A, B], Map[A, C]] =
    pure[F, Map[A, B], Map[A, C]] { in: Map[A, B] =>
      in.toList.traverse { case (key, value) =>
        rule.prefix(key).apply(value).map(_.map(key -> _))
      }.map(_.sequence).map(_.map(_.toMap))
    }
}

/** Type class instances for Rule */

trait RuleInstances {
  self: BaseRules =>
  implicit def ruleApplicative[F[_] : Applicative, A]: Applicative[Rule[F, A, ?]] =
    new Applicative[Rule[F, A, ?]] {
      def pure[B](value: B): Rule[F, A, B] =
        Rule.pure(_ => Applicative[F].pure(Ior.right(value)))

      def ap[B, C](funcRule: Rule[F, A, B => C])(argRule: Rule[F, A, B]): Rule[F, A, C] =
        (funcRule zip argRule) map { pair =>
          val (func, arg) = pair
          func(arg)
        }

      override def map[B, C](rule: Rule[F, A, B])(func: B => C): Rule[F, A, C] =
        rule map func

      override def product[B, C](rule1: Rule[F, A, B], rule2: Rule[F, A, C]): Rule[F, A, (B, C)] =
        rule1 zip rule2
    }


  //  implicit val ruleProfunctor: Profunctor[Rule] =
  //    new Profunctor[Rule] {
  //      override def dimap[A, B, C, D](fab: Rule[A, B])(f: (C) => A)(g: (B) => D) =
  //        fab.contramap(f).map(g)
  //    }
}
