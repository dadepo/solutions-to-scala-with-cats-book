package solutions.chapter4

import cats.data.Reader
import cats._
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

class Exercise_4_8_3Test extends FlatSpec with Matchers {

  "Chapter four, exercise 4.8.3" should "Implement reader monad" in {

    type DbReader[A] = Reader[Db, A]
    // usernames Map of userid to username
    // passwords Map of username to password
    case class Db(usernames: Map[Int, String], passwords: Map[String, String])

    def findUsername(userId: Int): DbReader[Option[String]] = Reader {db:Db => db.usernames.get(userId)}
    def checkPassword(username: String, password: String): DbReader[Boolean] = Reader {
      db:Db => db.passwords.get(username).contains(password)
    }

    def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
      username <- findUsername(userId)
      allowed <- username.map{uname => checkPassword(uname, password)}.getOrElse(false.pure[DbReader])
    } yield allowed


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

    val db = Db(users, passwords)
    assert(checkLogin(1, "zerocool").run(db))
    assert(!checkLogin(4, "davinci").run(db))
  }

}