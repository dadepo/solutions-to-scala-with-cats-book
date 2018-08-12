package solutions.chapter4

import cats._
import cats.data.Writer
import cats.implicits._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.duration._

import scala.concurrent.{Await, Future}

class Exercise_4_7_3Test extends FlatSpec with Matchers {

  /**
    * Rewrite factorial so it captures the log messages in a Writer. Demonstrate
    * that this allows us to reliably separate the logs for concurrent computations.
    */
  "Chapter four, exercise 4.7.3" should "Implement writer monad" in {

    // use to give impression that an execution is slow
    def slowly[A](body: => A) = try body finally Thread.sleep(100)

    def factorial(flag: String, n: Int): Int = {
      val ans = slowly(if(n == 0) 1 else n * factorial(flag, n - 1))
      println(s"$flag - fact $n $ans")
      ans
    }

    type Logged[A] = Writer[Vector[String], A]
    def factorialWithLogs(flag: String, n: Int): Logged[Int] = for {
      ans <- if(n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorialWithLogs(flag, n - 1).map(_ * n))
      }
      _ <- Vector(s"$flag - fact $n $ans").tell
    } yield ans


    type Log[A] = Writer[Vector[String], A]
    def factorialWithLogging(flag: String, n: Int):Log[Int] = {
      val computationWithWriter:Log[Int] = if (n == 0) {
        1.pure[Log]
      } else {
        slowly(factorialWithLogging(flag, n - 1).map(_ * n))
      }

      for {
        value <- computationWithWriter
        _ <- Vector(s"$flag - fact $n $value").tell
      } yield value
    }

    // running this will interleave the logging
    println {
      Await.result(Future.sequence(Vector(
        Future(factorial("a", 4)),
        Future(factorial("b", 3))
      )), 5.seconds)
    }

    // will attach the meta, log information to the results of each computation
    assert {
      Await.result(Future.sequence(Vector(
        Future(factorialWithLogs("a", 4).run),
        Future(factorialWithLogs("b", 3).run)
      )), 5.seconds) == Vector((Vector("a - fact 0 1", "a - fact 1 1", "a - fact 2 2", "a - fact 3 6", "a - fact 4 24"), 24), (Vector("b - fact 0 1", "b - fact 1 1", "b - fact 2 2", "b - fact 3 6"),6))
    }

    // will attach the meta, log information to the results of each computation
    assert {
      Await.result(Future.sequence(Vector(
        Future(factorialWithLogging("a", 4).run),
        Future(factorialWithLogging("b", 3).run)
      )), 5.seconds) == Vector((Vector("a - fact 0 1", "a - fact 1 1", "a - fact 2 2", "a - fact 3 6", "a - fact 4 24"), 24), (Vector("b - fact 0 1", "b - fact 1 1", "b - fact 2 2", "b - fact 3 6"),6))

    }
  }

}