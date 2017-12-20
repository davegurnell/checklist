package checklist

import cats.Order

sealed abstract class Path {
  def pathString: String = this match {
    case PNil               => ""
    case PField(head, PNil) => s"$head"
    case PField(head, tail) => s"$head/${tail.pathString}"
    case PIndex(head, PNil) => s"$head"
    case PIndex(head, tail) => s"$head/${tail.pathString}"
  }

  def prefix[A](prefix: A)(implicit format: PathPrefix[A]) =
    format.prefix(prefix, this)

  def ::[A](prefix: A)(implicit format: PathPrefix[A]) =
    format.prefix(prefix, this)

  def ++(that: Path): Path = this match {
    case PNil => that
    case PField(field, rest) => PField(field, rest ++ that)
    case PIndex(index, rest) => PIndex(index, rest ++ that)
  }

  override def toString = s"Path($pathString)"
}

object Path extends PathInstances

case object PNil extends Path
final case class PField(head: String, tail: Path = PNil) extends Path
final case class PIndex(head: Int, tail: Path = PNil) extends Path

final case class PathPrefix[A](func: (A, Path) => Path) {
  def prefix(value: A, path: Path): Path =
    func(value, path)

  def path(value: A): Path =
    prefix(value, PNil)

  def contramap[B](zoom: B => A): PathPrefix[B] =
    PathPrefix((b, path) => func(zoom(b), path))
}

object PathPrefix {
  def apply[A](implicit prefix: PathPrefix[A]): PathPrefix[A] =
    prefix

  def pure[A](func: (A, Path) => Path): PathPrefix[A] =
    PathPrefix(func)

  implicit val string: PathPrefix[String] =
    pure((field, path) => PField(field, path))

  implicit val int: PathPrefix[Int] =
    pure((index, path) => PIndex(index, path))

  implicit val path: PathPrefix[Path] =
    pure((prefix, path) => prefix ++ path)

  implicit val seqString: PathPrefix[Seq[String]] =
    pure((fields, path) => fields.foldRight(path)(PField.apply))

  implicit def prefixToPath[A](value: A)(implicit prefixer: PathPrefix[A]): Path =
    prefixer.path(value)
}

trait PathInstances {
  import cats.instances.string._
  implicit val pathOrder: Order[Path] = Order.by[Path, String](_.pathString)
}