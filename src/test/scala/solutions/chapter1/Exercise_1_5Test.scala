package solutions.chapter1

import cats._
import cats.implicits._

//import cats.implicits.{ catsSyntaxEq => _, _ }
import org.scalatest.{FlatSpec, Matchers}

/**
  * Implement an instance of Eq for our running Cat example:
  */
class Exercise_1_5Test extends FlatSpec with Matchers  {

  "Chapter one, exercise 1.5" should "Implement an instance of Eq for our running Cat example:" in {
    final case class Cat(name: String, age: Int, color: String)

    object Cat {
      implicit object catEq extends Eq[Cat] {
        override def eqv(firstCat: Cat, secondCat: Cat): Boolean = {
          Eq[Int].eqv(firstCat.age, secondCat.age) &&
            Eq[String].eqv(firstCat.name, secondCat.name) &&
            Eq[String].eqv(firstCat.color, secondCat.color)
        }
      }
    }

    val cat1 = Cat("Garfield", 38, "orange and black")
    val cat2 = Cat("Heathcliff", 32, "orange and black")

    assert(eq(cat1, cat2) == false)
    assert(cat1 =!= cat2 == true)
  }
}
