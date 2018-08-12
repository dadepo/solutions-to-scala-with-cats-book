package solutions.chapter5

import cats._
import cats.implicits._
import cats.data.EitherT
import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.{Await, Future}

class Exercise_5_4Test extends FlatSpec with Matchers {

  "Chapter four, exercise 5.4".should("use monad transformer").in({

    type Response[A] = EitherT[Future, String, A]

    val powerLevels = Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10
    )

    /**
      * implementing getPowerLevel to retrieve data from a
      * set of imaginary allies.
      */
    def getPowerLevel(autobot: String): Response[Int] = {
      powerLevels.get(autobot) match {
        case Some(value) => EitherT(Future.successful(value.asRight[String]))
        case None => data.EitherT(Future.successful(s"${autobot} not found".asLeft[Int]))
      }
    }

    // Using a different syntax
    def getPowerLevel2(autobot: String): Response[Int] = {
      powerLevels.get(autobot) match {
        case Some(value) => EitherT.right(Future.successful(value))
        case None => EitherT.left(Future.successful(s"${autobot} not found"))
      }
    }



    // Assert that the function to get the power levels, given the name of an alley works
    getPowerLevel("Jazz").value.map(level => assert(level == Right(6)))
    getPowerLevel("Jazzz").value.map(level => assert(level == Left(s"Jazzz not found")))
    getPowerLevel2("Jazz").value.map(level => assert(level == Right(6)))
    getPowerLevel2("Jazzz").value.map(level => assert(level == Left(s"Jazzz not found")))


    /**
      * Two autobots can perform a special move if their combined power level is
      * greater than 15. Write a second method, canSpecialMove, that accepts the
      * names of two allies and checks whether a special move is possible. If either
      * ally is unavailable, fail with an appropriate error message:

      */
    def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
      for {
        p1 <- getPowerLevel(ally1)
        p2 <- getPowerLevel(ally1)
      } yield p1 + p2 > 15
    }

    // using plain old flatmap
    def canSpecialMove2(ally1: String, ally2: String): Response[Boolean] = {
      getPowerLevel(ally1)
        .flatMap(p1 => getPowerLevel2(ally2).map(p2 => p1 + p2 > 15))
    }


    // Assert that the function to detect if it is possible for two allies to do special move works
    canSpecialMove("Jazz", "Bumblebee").value.map(canSpecialMove => assert(canSpecialMove == Right(false)))
    canSpecialMove("Jazz", "Hot Rod").value.map(canSpecialMove => assert(canSpecialMove == Right(true)))
    canSpecialMove2("Jazz", "Bumblebee").value.map(canSpecialMove => assert(canSpecialMove == Right(false)))
    canSpecialMove2("Jazz", "Hot Rod").value.map(canSpecialMove => assert(canSpecialMove == Right(true)))


    /**
      * Finally, write a method tacticalReport that takes two ally names and prints
      * a message saying whether they can perform a special move:
      */
    def tacticalReport(ally1: String, ally2: String): String = {
      val value: Future[Either[String, Boolean]] = canSpecialMove(ally1, ally2).value
      Await.result(value, 1.second) match {
        case Left(msg) =>
          s"Comms error: $msg"
        case Right(true) =>
          s"$ally1 and $ally2 are ready to roll out!"
        case Right(false) =>
          s"$ally1 and $ally2 need a recharge."
      }
    }

    assert(tacticalReport("Jazz", "Bumblebee") == "Jazz and Bumblebee need a recharge.")
    assert(tacticalReport("Bumblebee", "Hot Rod") == "Bumblebee and Hot Rod are ready to roll out!")
    assert(tacticalReport("Jazz", "Ironhide") == "Jazz and Ironhide need a recharge.")

  })
}
