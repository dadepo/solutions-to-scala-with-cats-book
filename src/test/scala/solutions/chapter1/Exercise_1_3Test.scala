package solutions.chapter1

import java.awt.print
import java.awt.print.Printable

import org.scalatest.{FlatSpec, Matchers}

/**
  * Scala provides a toString method to let us convert any value to a String.
  * However, this method comes with a few disadvantages: it is implemented for
  * every type in the language, many implementations are of limited use, and we
  * can’t opt-in to specific implementations for specific types.
  * Let’s define a Printable type class to work around these problems:
  * 1. Define a type class Printable[A] containing a single method format.
  * format should accept a value of type A and return a String.
  * 2. Create an object PrintableInstances containing instances of
  * Printable for String and Int.
  * 3. Define an object Printable with two generic interface methods:
  * format accepts a value of type A and a Printable of the corresponding
  * type. It uses the relevant Printable to convert the A to a String.
  * print accepts the same parameters as format and returns Unit. It
  * prints the A value to the console using println.
  * 4. Create a Cat type final case class Cat(name: String, age: Int, color: String)
  * and print is as Cat as {NAME} is a {AGE} year-old {COLOR} cat
  *
  *
  * This exercise can also be used to demonstrate the various ways that an instance of a type class can be created
  * 1. Via an anonymous instance of the type class - that is implicit val ti = new TypeClass[T]
  * 2. Via an object extended the type class - that is implicit object extends TypeClass[String]
  * 3. And if the instance depends on another instance, via recursive definition. This involves implicit def
  *
  * This exercise can also be used to demonstrate implicit resolution and the various locations implicit
  * can be found.
  */
class Exercise_1_3Test extends FlatSpec with Matchers  {

  "Chapter one, exercise 1.3" should "show various method for using typeclass instances" in {


    // The object to work with
    final case class Cat(name: String, age: Int, color: String)

    // The typeclass
    trait Printable[A] {
      def format(value:A): String
    }

    // An object that contains instances of Printable
    // There are various places where implicit instances can be put and retrieved
    // For more info, see: http://www.geekabyte.io/2017/12/implicit-scope-and-implicit-resolution.html
    object PrintableInstances {
      implicit object stringPrintable extends Printable[String] {
        override def format(value: String): String = value.toString
      }
      implicit object intPrintable extends Printable[Int] {
        override def format(value: Int): String = value.toString
      }

      // instance via object extending Printable[Cat] trait
      // Because this instance is defined by object it would always be selected and not the others below
      // Do not believe me? Comment out this and run the test, and you will get "ambiguous implicit values"

      // Why this is so? I do not know yet
      implicit object catPrintable extends Printable[Cat] {
        override def format(value: Cat): String = {
          s"Cat with name ${value.name} is a ${value.age} year-old ${value.color} cat"
        }
      }
      implicit val catPrintableVal = new Printable[Cat] {
        override def format(value: Cat): String = {
          s"Cat with name ${value.name} is a ${value.age} year-old ${value.color} cat"
        }
      }

      implicit def catPrintable(implicit intPrint: Printable[Int], stringPrint: Printable[String]): Printable[Cat] = (cat: Cat) => {
        val name = stringPrint.format(cat.name)
        val age = intPrint.format(cat.age)
        val color = stringPrint.format(cat.color)
        s"Cat with name $name is a $age year-old $color cat"
      }

      // Adds print to Cat providing a mechanism to resolve the typeclass instance for printable for cat
      implicit class PrintableSyntax[A](value: A) {
        def print(implicit printableInstance: Printable[A]): String = {
          printableInstance.format(value)
        }
      }
    }


    // There are various ways to use the typeclass instance to operate on a value
    // For more information see http://www.geekabyte.io/2017/12/common-forms-of-type-class-pattern-in.html

    // printable object, exposes method which allows us to take
    // 1. The object we want to work on
    // 2. Resolve the type class for that object
    // 3. Then use the type class instance on the method
    object Printable {

      // basic definition explicitly stating the instance as an implicit parameter
      def formatBasic[A](value:A)(implicit printer:Printable[A]): String = {
        printer.format(value)
      }

      // using context bound to enforce that an instance of printable must be in scope
      def formatContextBound[A: Printable](value: A): String = {
        implicitly[Printable[A]].format(value)
      }

      // providing a way to fish out the typeclass instance
      def apply[A](implicit instance: Printable[A]): Printable[A] = {
        instance
      }
    }


    // bring all the instance into scope
    import PrintableInstances._

    assert(Printable.formatBasic(Cat("kitty", 5, "brown")) == "Cat with name kitty is a 5 year-old brown cat")
    assert(Printable.formatContextBound(Cat("tom", 5, "white")) == "Cat with name tom is a 5 year-old white cat")
    assert(Printable[Cat].format(Cat("alinko", 5, "green")) == "Cat with name alinko is a 5 year-old green cat")
    assert(Cat("molala", 15, "blue").print == "Cat with name molala is a 15 year-old blue cat")
  }

}
