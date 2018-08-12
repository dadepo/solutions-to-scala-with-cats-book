package solutions.chapter7

import cats._
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * The exercises on traverse requires no implementation. It was more like, given this implementation, if you run it,
  * what will the answer be. Because of this, I decided to re-implement the custom list traverse and sequence given
  * in the book
  */
class TraverseTest extends FlatSpec with Matchers {
  "Chapter 7, exercise on traverse" should "implement custom sequence/travese for list" in {
    // Self given assignment: Replicate the implementation of traverse and sequence
    def customListTraverse[F[_]: Applicative, A, B](list: List[A], f: A => F[B]) : F[List[B]] = {
      list.foldLeft(Applicative[F].pure(List.empty[B]))((acc: F[List[B]], value: A) => {
        (acc, f(value)).mapN((_: List[B]) :+ (_: B))
      })
    }

    def customListSequence[F[_]: Applicative, A](lists: List[F[A]]) : F[List[A]] = {
      lists.foldLeft(Applicative[F].pure(List.empty[A]))((acc: F[List[A]], value: F[A]) => {
        (acc, value).mapN((_: List[A]) :+ (_: A))
      })
    }

    val hostnames = List(
      "alpha.example.com",
      "beta.example.com",
      "gamma.demo.com")


    def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)
    val totalUptime: Future[List[Int]] = customListTraverse(hostnames, getUptime)
    customListTraverse(hostnames, getUptime).map(lists => assert(lists == List(1020, 960, 840)))

  }
}
