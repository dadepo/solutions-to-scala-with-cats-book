package solutions.chapter4

import cats._
import cats.data.State
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}


/**
  * The State monad allows us to implement simple interpreters for complex expressions,
  * passing the values of mutable registers along with the result. We
  * can see a simple example of this by implementing a calculator for post-order
  * integer arithmetic expressions.
  * In case you haven’t heard of post-order expressions before (don’t worry if you
  * haven’t), they are a mathematical notation where we write the operator a􀁛er
  * its operands. So, for example, instead of writing 1 + 2 we would write:
  * 1 2 +
  * Although post-order expressions are difficult for humans to read, they are easy
  * to evaluate in code. All we need to do is traverse the symbols from le􀁛 to right,
  * carrying a stack of operands with us as we go:
  * • when we see a number, we push it onto the stack;
  * • when we see an operator, we pop two operands off the stack, operate
  * on them, and push the result in their place.
  * This allows us to evaluate complex expressions without using parentheses. For
  * example, we can evaluate (1 + 2) * 3) as follows:
  * 1 2 + 3 * // see 1, push onto stack
  * 2 + 3 * // see 2, push onto stack
  * + 3 * // see +, pop 1 and 2 off of stack,
  * // push (1 + 2) = 3 in their place
  * 3 3 * // see 3, push onto stack
  * 3 * // see 3, push onto stack
  * * // see *, pop 3 and 3 off of stack,
  * // push (3 * 3) = 9 in their place
  */
class Exercise_4_9_3Test extends FlatSpec with Matchers {

  "Chapter four, exercise 4.9.3" should "Implement state monad" in {
    type CalcState[A] = State[List[Int], A]

    def evalOne(sym:String) : CalcState[Int] = State {state:List[Int] =>
      sym match {
        case "+" => state match {
          case a :: b :: _ => (List(a + b), a + b)
          case _ => throw new Exception
        }
        case "-" => state match {
          case a :: b :: _ => (List(b - a), b - a)
          case _ => throw new Exception
        }
        case "*" => state match {
          case a :: b :: _ => (List(b * a), b * a)
          case _ => throw new Exception
        }
        case "/" => state match {
          case a :: b :: _ => (List(a/b), a/b)
          case _ => throw new Exception
        }
        case _ => (state :+ sym.toInt, sym.toInt)
      }
    }

    def evalAll(input: List[String]): CalcState[Int] =
      input.foldLeft(State{s:List[Int] => (s, 0)}) { (a, b) =>
        for {
          _ <- a
          ans <- evalOne(b)
        } yield ans
      }

    def evalInput(input: String): Int =
      evalAll(input.split(" ").toList).runA(Nil).value

    assert(evalInput("1 2 + 3 *") == 9)
    assert(evalInput("1 2 + 3 * 1 + 2 * 5 - -1 * 5 / 2 +") == 5)
  }

}