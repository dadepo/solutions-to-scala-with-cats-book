package solutions.chapter2

/**
  * When thinking about Monoids for boolean what I need to focus on is the empty value,
  * because all the operations possible on booleans would always leads to a boolean anyways and also in such a way
  * that the associative law is preserved...so whatever boolean
  * operator I come up with, &&, !, || etc would satisfy the combine requirement. It is the empty that goes with
  * these operator that ends up being the differentiators and ends up determining if the operator can form a monoid
  * with boolean.
  *
  *
  * We’ve seen a few examples of monoids but there are plenty more to be found.
  * Consider Boolean. How many monoids can you define for this type? For each
  * monoid, define the combine and empty operations and convince yourself that
  * the monoid laws hold. Use the following definitions as a starting point:
  *
  */
class Exercise_2_3Test {

  trait Monoid[A] {
    def combine(a:A, b:A): A
    def empty:A
  }

  implicit object booleanAndMonoid extends Monoid[Boolean] {
    override def combine(a: Boolean, b: Boolean): Boolean = a && b
    override def empty: Boolean = true
  }

  implicit object booleanOrMonoid extends Monoid[Boolean] {
    override def combine(a: Boolean, b: Boolean): Boolean = a || b
    override def empty: Boolean = false
  }

  implicit object booleanExclusiveOrMonoid extends Monoid[Boolean] {
    override def combine(a: Boolean, b: Boolean): Boolean = (a && !b) || (!a && b)
    override def empty: Boolean = false
  }

  implicit object booleanExclusiveNOrMonoid extends Monoid[Boolean] {
    override def combine(a: Boolean, b: Boolean): Boolean = (!a || b) && (a || !b)
    override def empty: Boolean = true
  }


}
