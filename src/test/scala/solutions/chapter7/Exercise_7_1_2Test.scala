package solutions.chapter7

import org.scalatest.{FlatSpec, Matchers}

/**
  * Try using foldLeft and foldRight with an empty list as the accumulator and
  * :: as the binary operator. What results do you get in each case?
  */
class Exercise_7_1_2Test extends FlatSpec with Matchers {

  "Chapter 7, exercise 7.1.2" should "implement foldLeft/foldRight with empty list and ::" in {
    def ffoldLeft[A](structure: List[A]): List[A] = {
      structure.foldLeft(List[A]())((acc: List[A], s: A) => s :: acc)
    }

    def ffoldRight[A](structure: List[A]): List[A] = {
      structure.foldRight(List[A]())((s: A, acc: List[A]) => s :: acc)
    }

    assert(ffoldLeft(List(1,2,3)) == List(3, 2, 1))
    assert(ffoldRight(List(1,2,3)) == List(1, 2, 3))
  }
}
