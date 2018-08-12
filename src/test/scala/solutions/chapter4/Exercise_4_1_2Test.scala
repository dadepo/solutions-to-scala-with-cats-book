package solutions.chapter4

import org.scalatest.{FlatSpec, Matchers}

class Exercise_4_1_2Test extends FlatSpec with Matchers {

  "Chapter four, exercise 4.1.2" should "define map using pure and flatMap" in {
    /**
      * Define Map with methods from Monad
      */
    trait MYMonad[F[_]] {
      def pure[A](a: A): F[A]
      def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
      // Map defined using pure and flapMap
      def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)((x: A) => pure(func(x)))
    }

    val MYMonadInstance = new MYMonad[Option] {
      override def pure[A](a: A): Option[A] = Option(a)
      override def flatMap[A, B](value: Option[A])(func: A => Option[B]): Option[B] = value.flatMap(func)
    }

    assert(MYMonadInstance.pure(1) == Option(1))
    assert(MYMonadInstance.flatMap(Option(1))(x => Option(x + 1)) == Option(2))
  }

}