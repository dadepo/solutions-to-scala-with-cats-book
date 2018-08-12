package solutions.chapter1

import cats._
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Re-implement the Cat application from the previous section using Show instead
  * of Printable.
  *
  *   final case class Cat(name: String, age: Int, color: String)
  *   s"Cat as ${value.name} is a ${value.age} year-old ${value.color} cat"
  */
class Exercise_1_4Test extends FlatSpec with Matchers  {

  "Chapter one, exercise 1.4" should "use show typeclass from cats" in {
    final case class Cat(name: String, age: Int, color: String)
    // Putting the implicit instances into the cats companion object this time
    object Cat {
      // recursive derivation of show instance for Cats, as long as there instance for Int and String
      implicit def showCat(implicit intShow: Show[Int], stringShow: Show[String]): Show[Cat] = (cat: Cat) => {
        val catName = stringShow.show(cat.name)
        val catAge = intShow.show(cat.age)
        val catColor = stringShow.show(cat.color)
        s"Cat with name ${cat.name} is a ${catAge} year-old ${catColor} cat"
      }
    }
    //using cat's show instance by direct retrieval via apply
    assert(Show[Cat].show(Cat("kitty", 5, "brown")) == "Cat with name kitty is a 5 year-old brown cat")
    // using cat's show instance by syntax
    assert(Cat("kitty", 5, "brown").show == "Cat with name kitty is a 5 year-old brown cat")
  }
}
