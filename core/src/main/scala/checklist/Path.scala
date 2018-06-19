package checklist

import cats.{Monoid, Order}

sealed abstract class Path {
  def pathString: String = this match {
    case PNil               => ""
    case PField(head, PNil) => s"$head"
    case PField(head, tail) => s"$head/${tail.pathString}"
    case PIndex(head, PNil) => s"$head"
    case PIndex(head, tail) => s"$head/${tail.pathString}"
  }

  def prefix[A](prefix: A)(implicit format: ToPath[A]): Path =
    format.prefix(prefix, this)

  def ::[A](prefix: A)(implicit format: ToPath[A]): Path =
    format.prefix(prefix, this)

  def ++(that: Path): Path = this match {
    case PNil => that
    case PField(field, rest) => PField(field, rest ++ that)
    case PIndex(index, rest) => PIndex(index, rest ++ that)
  }

  override def toString = s"Path($pathString)"
}

case object PNil extends Path
final case class PField(head: String, tail: Path = PNil) extends Path
final case class PIndex(head: Int, tail: Path = PNil) extends Path

object Path extends PathInstances

trait PathInstances {
  import cats.instances.string._

  implicit val pathOrder: Order[Path] =
    Order.by[Path, String](_.pathString)

  implicit val pathMonoid: Monoid[Path] =
    new Monoid[Path] {
      override def empty: Path =
        PNil

      override def combine(x: Path, y: Path): Path =
        x ++ y
    }
}