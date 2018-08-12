package solutions.chapter3

import java.awt.print.Printable

import cats.kernel.Monoid
import cats.{Functor, Invariant}
import org.scalatest.{FlatSpec, Matchers}

class Exercise_3_6_1_1Test extends FlatSpec with Matchers  {
  /**
    * Implement the contramap method for Printable
    * trait Printable[A] {
    * def format(value: A): String
    * }
    */

  "Chapter three, exercise 3.6.1.1" should "implement contramap for printable. Use with Box[T] via implicit def" in {

    trait Printable[A] { self =>
      def format(value: A): String
      // the implementation
      def contramap[B](ifunc: B => A): Printable[B] = {
        new Printable[B] {
          override def format(value: B): String = self.format(ifunc(value))
        }
      }
    }

    // given a box type
    final case class Box[A](value: A)

    // and given an instance of printable for String
    implicit val stringPrintable: Printable[String] =
      new Printable[String] {
        def format(value: String): String = value
      }

    // and also an instance of printable for Boolean
    implicit val booleanPrintable: Printable[Boolean] =
      new Printable[Boolean] {
        def format(value: Boolean): String = if(value) "yes" else "no"
      }

    /**
      * Now define an instance of Printable for the following Box case class. You’ll
      * need to write this as an implicit def
      */
    implicit def printableBox[A](implicit printableA: Printable[A]): Printable[Box[A]] = {
      new Printable[Box[A]] {
        override def format(boxValue: Box[A]): String = printableA.format(boxValue.value)
      }
    }

    // using the printable typeclass
    def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

    assert(format("somevalue") == "somevalue")
    assert(format(true) == "yes")
    assert(format(false) == "no")

    assert(format(Box("somevalue")) == "somevalue")
    assert(format(Box(true)) == "yes")
    assert(format(Box(false)) == "no")
  }

  "Chapter three, exercise 3.6.1.1" should "implement contramap for printable. Use with Box[T] via contramap def" in {

    trait Printable[A] { self =>
      def format(value: A): String
      // the implementation
      def contramap[B](ifunc: B => A): Printable[B] = {
        new Printable[B] {
          override def format(value: B): String = self.format(ifunc(value))
        }
      }
    }

    // given a box type
    final case class Box[A](value: A)

    // and given an instance of printable for String
    implicit val stringPrintable: Printable[String] =
      new Printable[String] {
        def format(value: String): String = value
      }

    // and also an instance of printable for Boolean
    implicit val booleanPrintable: Printable[Boolean] =
      new Printable[Boolean] {
        def format(value: Boolean): String = if(value) "yes" else "no"
      }

    /**
      * Rather than writing out the complete definition from scratch (new Printable[
      * Box] etc…), create your instance from an existing instance using contramap.
      *
      * The idea of contramap is this.
      *
      * I know how to do something on A
      * I need to do something on a B, but I do not know how
      * I have a function that can convert my B to A
      *
      * Hence I should be able to do the stuff on B. How? By first converting it to A
      */
    implicit def boxPrintableViaContramap[A](implicit printableA: Printable[A]) : Printable[Box[A]] = {
      val f: Box[A] => A = (box: Box[A]) => box.value //function to go from box to A
      printableA.contramap(f)
    }


    // using the printable type class
    def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

    assert(format("somevalue") == "somevalue")
    assert(format(true) == "yes")
    assert(format(false) == "no")

    assert(format(Box("somevalue")) == "somevalue")
    assert(format(Box(true)) == "yes")
    assert(format(Box(false)) == "no")
  }
}
