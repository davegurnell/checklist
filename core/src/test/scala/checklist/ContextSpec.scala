package checklist

import cats._
import cats.data.Ior
import cats.effect.IO
import cats.implicits._
import checklist.Message.errors
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{AsyncFreeSpec, Matchers}

import scala.collection.immutable.HashSet
import scala.concurrent.Future

class ContextSpec extends AsyncFreeSpec with Matchers {
  implicit override def executionContext: Scheduler = monix.execution.Scheduler.Implicits.global

  val rule1 = Rule.pass[Int]
  val rule2 = rule1.liftTo[Future]

  val db = HashSet("user1@example.com", "user2@example.com")
  def existsInDBFuture(email: String): Future[Boolean] = Future.successful(db.contains(email))
  def existsInDBTask(email: String): Task[Boolean] = Task(db.contains(email))
  def existsInDBIO(email: String): IO[Boolean] = IO(db.contains(email))

  val isUniqueFuture =
    Rule.test(Message.errors("Email is already registered")) { email: String => existsInDBFuture(email).map(!_) }
  val isUniqueTask =
    Rule.test(Message.errors("Email is already registered")) { email: String => existsInDBTask(email).map(!_) }
  val isUniqueIO =
    Rule.test(Message.errors("Email is already registered")) { email: String => existsInDBIO(email).map(!_) }

  val isEmail = Rule.matchesRegexStrict("^[^@]+@[^@]+$".r, Message.errors("Must be an email"))
  val isEmailFuture = isEmail.liftTo[Future]

  val futureToTask: Future ~> Task = new (Future ~> Task) {
    override def apply[A](f: Future[A]): Task[A] = Task.deferFuture(f)
  }

  def taskToFuture: Task ~> Future = new (Task ~> Future) {
    override def apply[A](t: Task[A]): Future[A] = t.runAsync
  }

  val ioToFuture: IO ~> Future = new (IO ~> Future) {
    override def apply[A](i: IO[A]): Future[A] = i.unsafeToFuture
  }

  "and: lift id to future" in {
    val combined1 = rule1 and rule2
    val combined2 = rule2 and rule1
    combined1(+1) map (_ should be(Ior.right(+1)))
    combined2(+1) map (_ should be(Ior.right(+1)))
  }

  "andThen: lift id to future" in {
    val combined1 = rule1 andThen rule2
    val combined2 = rule2 andThen rule1
    combined1(+1) map (_ should be(Ior.right(+1)))
    combined2(+1) map (_ should be(Ior.right(+1)))
  }

  "zip: lift id to future" in {
    val combined1 = rule1 zip rule2
    val combined2 = rule2 zip rule1
    combined1(+1) map (_ should be(Ior.right((+1, +1))))
    combined2(+1) map (_ should be(Ior.right((+1, +1))))
  }

  "Future unique email" in {
    val validator = isEmail and isUniqueFuture
    validator("newuser@example.com") map (_ should be(Ior.right("newuser@example.com")))
    validator("user1@example.com") map (_ should be(Ior.left(errors("Email is already registered"))))
    validator("hello@") map (_ should be(Ior.left(errors("Must be an email"))))
  }

  "Task unique email" in {
    val validator = isEmail and isUniqueTask
    validator("newuser@example.com").runAsync map (_ should be(Ior.right("newuser@example.com")))
    validator("user1@example.com").runAsync map (_ should be(Ior.left(errors("Email is already registered"))))
    validator("hello@").runAsync map (_ should be(Ior.left(errors("Must be an email"))))
  }

  "Id, Future and Task unique non-empty email (as Task)" in {
    implicit val transformation: Future ~> Task = futureToTask

    val validator = Rule.nonEmpty[String] and isUniqueTask and isEmailFuture
    validator("newuser@example.com").runAsync map (_ should be(Ior.right("newuser@example.com")))
    validator("user1@example.com").runAsync map (_ should be(Ior.left(errors("Email is already registered"))))
    validator("hello@").runAsync map (_ should be(Ior.left(errors("Must be an email"))))
    validator("").runAsync map (_ should be(Ior.left(errors("Must not be empty", "Must be an email"))))
  }

  "Id, Future and Task unique non-empty email (as Future)" in {
    implicit val transformation: Task ~> Future = taskToFuture

    val validator = isUniqueTask and isEmailFuture and Rule.nonEmpty[String]
    validator("newuser@example.com") map (_ should be(Ior.right("newuser@example.com")))
    validator("user1@example.com") map (_ should be(Ior.left(errors("Email is already registered"))))
    validator("hello@") map (_ should be(Ior.left(errors("Must be an email"))))
    validator("") map (_ should be(Ior.left(errors("Must be an email", "Must not be empty"))))
  }

  "IO, Future and Task unique non-empty email (as Future)" in {
    implicit val transformation1: Task ~> Future = taskToFuture
    implicit val transformation2: IO ~> Future = ioToFuture

    val validator = isUniqueTask and isEmailFuture and Rule.nonEmpty[String].liftTo[IO]
    validator("newuser@example.com") map (_ should be(Ior.right("newuser@example.com")))
    validator("user1@example.com") map (_ should be(Ior.left(errors("Email is already registered"))))
    validator("hello@") map (_ should be(Ior.left(errors("Must be an email"))))
    validator("") map (_ should be(Ior.left(errors("Must be an email", "Must not be empty"))))
  }
}
