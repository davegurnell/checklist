 package checklist
 package std

 abstract class ToMessage[A] {
   def toError(value: A): Message
   def toWarning(value: A): Message
 }

 object ToMessage {
   implicit val stringToMessage: ToMessage[String] =
     new ToMessage[String] {
       def toError(message: String) = ErrorMessage(message)
       def toWarning(message: String) = WarningMessage(message)
     }

   implicit def prefixPairToMessage[P: ToPath](implicit prefix: ToPath[P]): ToMessage[(P, String)] =
     new ToMessage[(P, String)] {
       def toError(pair: (P, String)) = ErrorMessage(pair._2, prefix.path(pair._1))
       def toWarning(pair: (P, String)) = WarningMessage(pair._2, prefix.path(pair._1))
     }

   implicit def pathPairToMessage(implicit path: Path): ToMessage[(Path, String)] =
     new ToMessage[(Path, String)] {
       def toError(pair: (Path, String)) = ErrorMessage(pair._2, pair._1)
       def toWarning(pair: (Path, String)) = WarningMessage(pair._2, pair._1)
     }
 }
