package solutions.chapter2

import cats._
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

class Exercise_2_5_4Test extends FlatSpec with Matchers  {

  /**
    * The cutting edge SuperAdder v3.5a-32 is the world’s first choice for adding
    * together numbers. The main function in the program has signature def
    * add(items: List[Int]): Int. In a tragic accident this code is deleted!
    * Rewrite the method and save the day!
    */
  "Chapter two, exercise 2.5.4" should "implement add with signature List[Int] => Int" in {

    def addNonGeneric(items:List[Int]):Int = {
      items.foldLeft(0)(_ + _)
    }

    assert(addNonGeneric(List(1,2,3)) == 6)
  }

  /**
    * Well done! SuperAdder’s market share continues to grow, and now
    * there is demand for additional functionality. People now want to add
    * List[Option[Int]]. Change add so this is possible. The SuperAdder code
    * base is of the highest quality, so make sure there is no code duplication!
    */
  "Chapter two, exercise 2.5.4" should "implement add with signature List[Option[Int]] => Int" in {

    def add[A: Monoid](items:List[A]):A = {
      items.foldLeft(Monoid[A].empty)(_ |+| _)
    }

    assert(add(List(1,2,3)) == 6)
  }

  /**
    * SuperAdder is entering the POS (point-of-sale, not the other POS) market.
    * Now we want to add up Orders:
    * case class Order(totalCost: Double, quantity: Double)
    * We need to release this code really soon so we can’t make any modifications
    * to add. Make it so!
    */
  "Chapter two, exercise 2.5.4" should "implement monoid for Order and use with add" in {

    // The thing
    case class Order(totalCost: Double, quantity: Double)

    // Its typeclass instance
    implicit object OrderMonoid extends Monoid[Order] {
      override def empty: Order = Order(0,0)
      override def combine(x: Order, y: Order): Order = Order(
        x.totalCost + y.totalCost,
        x.quantity + y.totalCost
      )
    }

    // the operation defined in terms of the thing and its typeclass
    def add[A: Monoid](items:List[A]):A = {
      items.foldLeft(Monoid[A].empty)(_ |+| _)
    }

    assert(add(List(Order(1,1),Order(2,2), Order(3,3))) == Order(6,6))
    assert(add(List(Option(Order(1,1)),Option(Order(2,2)), Option(Order(3,3)), None)) == Option(Order(6,6)))
  }

}
