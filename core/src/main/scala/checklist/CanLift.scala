package checklist

import cats._
import scala.language.higherKinds


trait NaturalTransformationLowPriorityImplicits {
  implicit def applicativeTransform[F[_]](implicit evF: Applicative[F]): Id ~> F = new (Id ~> F) {
    override def apply[A](a: Id[A]): F[A] = evF.pure(a)
  }
}

trait NaturalTransformationImplicits extends NaturalTransformationLowPriorityImplicits {
  implicit def idTransform[F[_]]: F ~> F = new (F ~> F) {
    override def apply[A](fa: F[A]): F[A] = fa
  }

  implicit class LiftableOps[F[_], A](fa: F[A]) {
    def liftTo[G[_]](implicit transform: F ~> G): G[A] = transform(fa)
  }

}

trait CanLift[C[_[_]], F[_], G[_], R[_]] {
  val evR: C[R]
  def liftF: F ~> R
  def liftG: G ~> R
}

trait CanLiftMidPriorityImplicits extends NaturalTransformationImplicits {
  implicit def CanLiftToT[C[_[_]], F[_], G[_]](implicit evF: C[F], evG: C[G], transform: G ~> F, evCF: C[F] <:< Applicative[F], evCG: C[G] <:< Applicative[G]): CanLift[C, F, G, F] =
    new CanLift[C, F, G, F] {
      override val evR = evF
      override def liftF: F ~> F = idTransform
      override def liftG: G ~> F = transform
    }
}

trait CanLiftImplicits extends CanLiftMidPriorityImplicits {
  implicit def CanLiftToG[C[_[_]], F[_], G[_]](implicit evF: C[F], evG: C[G], transform: F ~> G, evCF: C[F] <:< Applicative[F], evCG: C[G] <:< Applicative[G]): CanLift[C, F, G, G] =
    new CanLift[C, F, G, G] {
      override val evR = evG
      override def liftF: F ~> G = transform
      override def liftG: G ~> G = idTransform
    }

  // I can't make this general approach work. It seems that the problem is requiring the implicit evidence for Applicative[R],
  // which forces the compiler to match some arbitrary R instead of leaving it free to determine after the transforms.
  // Compiling any "and" operation results in "diverging implicit expansion for type cats.kernel.Order[A]"
  //  implicit def canLiftToR[F[_] : Applicative, G[_] : Applicative, R[_] : Applicative](implicit transformF: F ~> R, transformG: G ~> R): CanLift[F, G, R] =
  //    new CanLift[F, G, R] {
  //      override val evApplicativeR = implicitly[Applicative[R]]
  //      override def liftF: F ~> R = transformF
  //      override def liftG: G ~> R = transformG
  //    }
}