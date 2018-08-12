package solutions.chapter2

import cats.kernel.Monoid

/**
  * What monoids and semigroups are there for sets?
  */
class Exercise_2_4Test {

  // Set union forms a monoid along with the empty set
  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]) = a union b
      def empty = Set.empty[A]
    }

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def combine(a: Int, b: Int) = a + b
    def empty = 0
  }

}
