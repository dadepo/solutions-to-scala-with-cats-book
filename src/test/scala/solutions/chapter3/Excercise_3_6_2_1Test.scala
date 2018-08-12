package solutions.chapter3

import org.scalatest.{FlatSpec, Matchers}

class Excercise_3_6_2_1Test extends FlatSpec with Matchers {

  "Chapter three, exercise 3.6.2.1" should "implement imap for codec" in {
    trait Codec[A] { self =>
      def encode(value: A): String
      def decode(value: String): A
      def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {

        override def encode(value: B): String = self.encode(enc(value))

        override def decode(value: String): B = dec(self.decode(value))
      }
    }

    // given an instance of code for String
    implicit val stringCodec: Codec[String] = new Codec[String] {
        def encode(value: String): String = value
        def decode(value: String): String = value
      }

    // using Imap I can get the instances for other types
    // like int
    implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)
    // like boolean
    implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)
    // like double
    implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)
    // like Box[Double]
    final case class Box[A](value: A)
    implicit def boxDoubleCodec(implicit doubleCodec: Codec[Double]): Codec[Box[Double]] = {
      val dec: Double => Box[Double] = Box(_)
      val enc: Box[Double] => Double  = _.value
      doubleCodec.imap(dec, enc)
    }


    def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
    def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

    assert(encode(123.4) == "123.4")
    assert(encode(Box(123.4)) == "123.4")
    assert(decode[Double]("123.4") == 123.4)
    assert(decode[Box[Double]]("123.4") == Box(123.4))


  }


}
