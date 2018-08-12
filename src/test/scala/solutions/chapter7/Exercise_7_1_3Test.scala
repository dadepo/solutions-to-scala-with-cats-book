package solutions.chapter7

import cats.Monoid
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

/**
  * foldLeft and foldRight are very general methods. We can use them to implement
  * many of the other high-level sequence operations we know. Prove
  * this to yourself by implementing substitutes for List's map, flatMap, filter,
  * and sum methods in terms of foldRight.
  */
class Exercise_7_1_3Test extends FlatSpec with Matchers {

  "Chapter 7, exercise 7.1.3" should "implement map/flatMap/filter in terms of foldRight" in {
    def lmap[A, B](list: List[A], f: A => B): List[B] = {
      list.foldRight(List.empty[B])((l: A, acc: List[B]) => f(l) :: acc)
    }

    def lflatmap[A,B](list: List[A], f: A => List[B]): List[B] = {
      list.foldRight(List.empty[B])((l: A, acc: List[B]) => f(l) ++ acc)
    }

    def lfilter[A](list: List[A], f: A => Boolean): List[A] = {
      list.foldRight(List.empty[A])((l: A, acc: List[A]) => if (f(l)) l :: acc else acc )
    }

    def lsum[A](list: List[A])(implicit monoid: Monoid[A]): A = {
      list.foldRight(monoid.empty)(monoid.combine)
    }

    def sumWithNumeric[A](list: List[A])(implicit numeric: Numeric[A]): A = list.foldRight(numeric.zero)(numeric.plus)


    // asserting that the map implemented in terms of foldRight works
    assert(lmap(List(1,2,3), (x:Int) => x * 2) == List(2,4,6))

    // asserting that the flatMap implemented in terms of foldRight works
    assert(lflatmap(List(1,2,3), (x:Int) => List(x * 2)) == List(2, 4, 6))

    // asserting that the filter implemented in terms of foldRight works
    assert(lfilter(List(1,2,3), (x:Int) => x == 2) == List(2))

    // asserting that the sum implemented with Numeric typeclass works
    assert(sumWithNumeric(List[Int](1,2,3)) == 6)

    // asserting that the sum implemented with foldRight works
    println(lsum(List[Int](1,2,3)))
  }
}
