package solutions.chapter4

import cats._
import cats.data.Reader
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Chapter 4 of the Scala with Cats book (https://underscore.io/books/scala-with-cats/) has got the
  * an exercise regarding the Reader Monad which is supposed to showcase the classic use case of Reader Monads:
  * which is to build programs
  * that accepts a configuration as parameter.
  *
  * The exercise can be summarised as follows.
  *
  * Given a DB configuration with the following shape:
  * case class Db(usernames: Map[Int, String], passwords: Map[String, String])
  * where usernames is Map of userid to username
  * and passwords is a Map of username to password
  *
  * Create a method that generate DbReaders to look up the username for an Int user ID. The method will look this this:
  * def findUsername(userId: Int): DbReader[Option[String]] = ???
  *
  * Create a method that looks up the password for a String username. The method will look like this:
  * def checkPassword(username: String, password: String): DbReader[Boolean] = ???
  *
  * Then making use of the findUsername and checkPassowrd method, create a checkLogin method to check the password for a given user
  * ID. The method should be as follows:
  *
  * def checkLogin(userId: Int, password: String): DbReader[Boolean] = ???
  *
  *
  * After reading about the Reader monad and implementing the exercise I had a feeling Reader monad is unnecessary in
  * Scala since the language has Implicits which can easily be used to achieve what is said to be the primary use
  * case of Reader Monads.
  *
  * This solution shows how to achieve the same using implicit parameters.
  *
  * This gist shows the three alternative solution https://gist.github.com/dadepo/4138175b64dca46ca64158727ca3278e
  */
class Exercise_4_8_3_ExtensionTest extends FlatSpec with Matchers {

  "Chapter four, exercise 4.8.3 Extension" should "Implement reader monad" in {

    // usernames Map of userid to username
    // passwords Map of username to password
    case class Db(usernames: Map[Int, String], passwords: Map[String, String])

    def findUsername(userId: Int)(implicit db: Db): Option[String] =
      db.usernames.get(userId)
    def checkPassword(username: String, password: String)(implicit db: Db): Boolean =
      db.passwords.get(username).contains(password)

    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )

    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )

    implicit val db: Db = Db(users, passwords)

    def checkLogin(userId: Int, password: String)(implicit db: Db): Boolean = {
      for {
        username <- findUsername(userId)
        allowed <- Some(checkPassword(username, password))
      } yield allowed
    }.getOrElse(false)

    assert(checkLogin(1, "zerocool"))
    assert(!checkLogin(4, "davinci"))

  }

}