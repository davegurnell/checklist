package checklist

sealed abstract class Path {
  def pathString: String = this match {
    case PNil               => ""
    case PField(head, PNil) => s"$head"
    case PField(head, tail) => s"$head/${tail.pathString}"
    case PIndex(head, PNil) => s"$head"
    case PIndex(head, tail) => s"$head/${tail.pathString}"
  }

  // TODO: Move this and the old `ValidationPath.unapply` method
  // to a separate `js` package:

  // def jsString: String = this match {
  //   case PNil                       => ""
  //   case PField(head, tail: PField) => s"$head.${tail.jsString}"
  //   case PField(head, tail)         => s"$head${tail.jsString}"
  //   case PIndex(head, tail: PField) => s"[$head.${tail.jsString}]"
  //   case PIndex(head, tail)         => s"[$head${tail.jsString}]"
  // }

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

case object PNil extends Path
final case class PField(head: String, tail: Path = PNil) extends Path
final case class PIndex(head: Int, tail: Path = PNil) extends Path

final case class PathPrefix[A](func: (A, Path) => Path) {
  def prefix(value: A, path: Path): Path =
    func(value, path)

  def path(value: A): Path =
    prefix(value, PNil)
}

object PathPrefix {
  def apply[A](implicit prefix: PathPrefix[A]): PathPrefix[A] =
    prefix

  def pure[A](func: (A, Path) => Path): PathPrefix[A] =
    PathPrefix(func)

  implicit val stringPrefixer: PathPrefix[String] =
    pure((field, path) => PField(field, path))

  implicit val intPrefixer: PathPrefix[Int] =
    pure((index, path) => PIndex(index, path))

  implicit val pathPrefixer: PathPrefix[Path] =
    pure((prefix, path) => prefix ++ path)

  implicit val seqStringPrefixer: PathPrefix[Seq[String]] =
    pure((fields, path) => fields.foldRight(path)(PField.apply))

  import scala.language.implicitConversions
  implicit def prefixToPath[A](value: A)(implicit prefixer: PathPrefix[A]): Path =
    prefixer.path(value)
}
