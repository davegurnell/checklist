package checklist

abstract class ToMessage[A] {
  def toMessage(value: A): Message
}

object ToMessage {
  implicit val stringToMessage: ToMessage[String] =
    new ToMessage[String] {
      def toMessage(message: String) = Message(message)
    }

  implicit def prefixPairToMessage[P: PathPrefix](implicit prefix: PathPrefix[P]): ToMessage[(P, String)] =
    new ToMessage[(P, String)] {
      def toMessage(pair: (P, String)) = Message(pair._2, prefix.path(pair._1))
    }

  implicit def pathPairToMessage(implicit path: Path): ToMessage[(Path, String)] =
    new ToMessage[(Path, String)] {
      def toMessage(pair: (Path, String)) = Message(pair._2, pair._1)
    }
}
