package solutions.chapter6

import cats._
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Implement product in terms of flatMap
  */
class Exercise_6_3_1_1Test extends FlatSpec with Matchers {

  "Chapter six, exercise 6.3.1.1" should "implement product in terms of flatMap" in {
    // Using the apply to retrieve the instances
    def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
      Monad[M].flatMap(x)(x => {Monad[M].map(y)(y => (x, y))})

    // Using the syntax approach to make use of the instances
    def product2[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
      x.flatMap(xx => y.map(yy => (xx, yy)))

    // Using for comprehension
    def product3[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = for {
      xx <- x
      yy <- y
    } yield (xx, yy)


    assert(product(Option(1), Option(2)) == Option((1,2)))
    assert(product2(Option(1), Option(2)) == Option((1,2)))
    assert(product3(Option(1), None) == None)
  }

}
