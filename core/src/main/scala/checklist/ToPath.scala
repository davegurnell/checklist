package checklist

final case class ToPath[A](func: (A, Path) => Path) {
  def prefix(value: A, path: Path): Path =
    func(value, path)

  def path(value: A): Path =
    prefix(value, PNil)

  def contramap[B](zoom: B => A): ToPath[B] =
    ToPath((b, path) => func(zoom(b), path))
}

object ToPath {
  def apply[A](implicit prefix: ToPath[A]): ToPath[A] =
    prefix

  def pure[A](func: (A, Path) => Path): ToPath[A] =
    ToPath(func)

  implicit val string: ToPath[String] =
    pure((field, path) => PField(field, path))

  implicit val int: ToPath[Int] =
    pure((index, path) => PIndex(index, path))

  implicit val path: ToPath[Path] =
    pure((prefix, path) => prefix ++ path)

  implicit val seqString: ToPath[Seq[String]] =
    pure((fields, path) => fields.foldRight(path)(PField.apply))

  implicit def prefixToPath[A](value: A)(implicit prefixer: ToPath[A]): Path =
    prefixer.path(value)
}
