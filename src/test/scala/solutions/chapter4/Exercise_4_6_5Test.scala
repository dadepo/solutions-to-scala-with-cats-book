package solutions.chapter4

import cats.Eval
import org.scalatest.{FlatSpec, Matchers}

class Exercise_4_6_5Test extends FlatSpec with Matchers {

  /**
    * The naive implementation of foldRight below is not stack safe. Make it so
    * using Eval:
    * def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    * as match {
    * case head :: tail =>
    * fn(head, foldRight(tail, acc)(fn))
    * case Nil =>
    * acc
    * }
    */
  "Chapter four, exercise 4.6.5" should "Implement stack safe foldLeft using Eval" in {

    // not stack-safe
    def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = as match {
        case head :: tail =>
          fn(head, foldRight(tail, acc)(fn))
        case Nil =>
          acc
      }

    //Indeed...it blows up the stack
    assertThrows[StackOverflowError] {
      println(foldRight(List.fill(30000)(2), 0)(_ + _))
    }

    def foldRightSafe[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
      as match {
        case head :: tail =>
          Eval.defer(fn(head, foldRightSafe(tail, acc)(fn)))
        case Nil =>
          acc
      }

    val value: Int = foldRightSafe(List.fill(300000)(2), Eval.now(0))((a,b) => b.map(_ + a)).value
    assert(value == 600000)



  }

}