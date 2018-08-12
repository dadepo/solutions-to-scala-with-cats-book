package solutions.chapter4

import org.scalatest.{FlatSpec, Matchers}

class Exercise_4_3_1Test extends FlatSpec with Matchers {

  "Chapter four, exercise 4.3.1" should "Implement pure, map, and flatMap for Id" in {

    import cats.Id
    def pure[A](value: A): Id[A] = value
    def map[A, B](initial: Id[A])(func: A => B): Id[B] = func(initial)
    def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)

    assert(pure(1) == 1)
    assert(map(pure(1))((_: Id[Int]) + 1) == 2)
    assert(flatMap(pure(1))((n: Id[Int]) => pure(n + 1)) == 2)
  }

}