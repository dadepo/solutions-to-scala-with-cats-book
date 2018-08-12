package solutions.chapter6

import cats._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

/**
  * We receive request data from the client in a Map[String, String]
  * and we want to parse it to create a User object:
  * case class User(name: String, age: Int)
  * Our goal is to implement code that parses the incoming data enforcing the
  * following rules:
  * • the name and age must be specified;
  * • the name must not be blank;
  * • the age must be a valid non-negative integer.
  */
class Exercise_6_4_4Test extends FlatSpec with Matchers {

  "Chapter 6, exercise 6.4.4" should "implement form validation using validated" in {
    type FormData = Map[String, String]
    type FailFastOr[A] = Either[List[String], A]
    type FailSlowOr[A] = Validated[List[String], A]

    case class User(name: String, age: Int)

    // utility method to read value of given key from supplied map
    def readValue(fieldName: String)(date: FormData): FailFastOr[String] = {
      date.get(fieldName)toRight List(s"$fieldName field not specified")
    }

    // sets of utility validating functions
    def parseInt(name:String)(intValue: String): FailFastOr[Int] = {
      Either
        .catchOnly[NumberFormatException](intValue.toInt)
        .leftMap((_: NumberFormatException) => List(s"$name must be an integer"))
    }
    def nonBlank(name:String)(data: String) : FailFastOr[String] = {
      if (data.nonEmpty) {
        Right(data)
      } else {
        Left(List(s"$name cannot be blank"))
      }
    }
    def nonNegative(name:String)(data: Int) : FailFastOr[Int] = {
      if (data >= 0) {
        Right(data)
      } else {
        Left(List(s"$name cannot be non negative"))
      }
    }

    // Reading the name value and validating using the validation functions above
    def readName(data: FormData): FailFastOr[String] =
      readValue("name")(data)
        .flatMap(nonBlank("name")(_))


    // Reading the age and validating using the validation functions above
    def readAge(data: FormData): FailFastOr[Int] = {
      readValue("age")(data)
        .flatMap(nonBlank("age")(_))
        .flatMap(parseInt("age")(_))
        .flatMap(nonNegative("age")(_))
    }

    def readUser(data: FormData): FailSlowOr[User] = {
      (readName(data).toValidated, readAge(data).toValidated).mapN(User.apply _)
    }

    assert(readUser(Map("name" -> "Dave", "age" -> "37")) == Valid(User("Dave", 37)))
    assert(readUser(Map("age" -> "-1")) == Invalid(List("name field not specified", "age cannot be non negative")))
    assert(readUser(Map("name"-> "","age" -> "-1")) == Invalid(List("name cannot be blank", "age cannot be non negative"))
    )
  }
}
