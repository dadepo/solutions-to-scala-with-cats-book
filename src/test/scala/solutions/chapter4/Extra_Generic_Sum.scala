package solutions.chapter4

import cats._
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class Extra_Generic_Sum extends FlatSpec with Matchers {

  "Extra exercise based on Chapter 4" should "Implement ageneric summing function based on Monads and Monoid" in {

    def genericSum[F[_]: Monad, T:Monoid](a: F[T], b: F[T]):F[T] = {
      a.flatMap(x => b.map( y => x |+| y))
    }

    assert(genericSum(Option(3), Option(2)) == Option(5))
    assert(genericSum(Try(3), Try(2)) == Try(5))
    assert(genericSum("Hello ".asRight[String], "World".asRight[String]) == Right("Hello World"))
  }

}
