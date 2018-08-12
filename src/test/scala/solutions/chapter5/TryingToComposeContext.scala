package solutions.chapter5

import cats.{Functor, Monad}
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

class TryingToComposeContext extends FlatSpec with Matchers {
  /**
    * The idea behind composing context is this:
    * I have a context A, eg A[something]
    * and I have another context B, eg B[Something]
    * and I want to be able to create context A[B[something]]
    *
    * So basically if I have a context A[Something] that has an instance of an Applicative and I also have another
    * context B[Something] that also has an instance of Applicative, then I
    * should be able to automatically use A[B[Something]] as an applicative. That is I should be able to synthesise
    * an Applicative for the context A[B[_]] from the applicative of A[_] and B[_]
    *
    * This is the idea behind composing typeclass instances. Functors compose, Applicative compose, but Monad does not
    */

  //  Attempt to have a method that automatically compose applicative.

  // implicit def composeApplicative[A[_]: Applicative, B[_]: Applicative](a:A[_],b:B[_]) = {
  //    type Compose[T] = A[B[T]]
  //    new Applicative[Compose] {
  //      override def pure[T](x: T): A[B[T]] = Applicative[A].pure(Applicative[B].pure(x))
  //      override def ap[A, B](ff: Compose[A => B])(fa: Compose[A]): Compose[B] = {
  //        ??? //attempting to manually compose Applicatives
  //      }
  //    }
  //  }



  // Composing Functors
  implicit def composeFunctor[F[_], G[_]](implicit functorF:Functor[F], functorG:Functor[G]): Functor[Lambda[A => F[G[A]]]] = {
    new Functor[Lambda[A => F[G[A]]]] {
      override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] = {
        functorF.map(fa)(gs => {
          functorG.map(gs)(f(_))
        })
      }
    }
  }

  // Trying to compose Monads and seeing that it is not possible unless the inner monad is specified.
  // so in this case the inner monad is specified to Option
  implicit def composeMonad[M[_]](implicit monadM:Monad[M], monadG:Monad[Option]): Monad[Lambda[A => M[Option[A]]]] = {
    new Monad[Lambda[A => M[Option[A]]]] {
      override def pure[A](x: A): M[Option[A]] = monadM.pure(monadG.pure(x))

      override def flatMap[A, B](ma: M[Option[A]])(f: A => M[Option[B]]): M[Option[B]] = {

        val y:M[Option[M[Option[B]]]] = ma.map((ga: Option[A]) => {
          ga.map((a: A) => f(a))
        })

        val z:M[M[Option[B]]] = ma.map((ga: Option[A]) => {
          ga.map((a: A) => f(a)).get
        })

        /**
          * we start with [M[G[T]]
          * If I map into T to apply my function, I get
          * [M[G[  M[G[Y]]  ]]
          * [M[G[Y]]
          *
          * Instead of Mapping into [M[G[T]] to get T, Ill map into M to get G[T]
          * if I can get read of G somehow (maybe G is option so i can call .get, then ill get [M[T]]
          * I can then apply my F so I get M[ M[G[Y]]  ]
          *
          * I have flatMap on M, so the two Ms can be flattened, to give M[G[Y]]
          */
        val result:M[Option[B]] = ma.flatMap((optionA: Option[A]) => {
          val value: A = optionA.get
          val valuex:M[Option[B]] = f(value)
          valuex
        })

        result
      }

      override def tailRecM[A, B](a: A)(f: A => M[Option[Either[A, B]]]): M[Option[B]] = ???
    }

  }


  type ListOfOption[Int] = List[Option[Int]]

  // Using type lambda to specify the type of the value variable.
  //  implicit class ListOfOptionInt(value: ({type Alias = List[Option[Int]]})#Alias) {
  //    def composedMap(f: Int => Int) = {
  //      composeFunctor(Functor[List], Functor[Option]).map(value)(f)
  //    }
  //  }

  implicit class ListOfOptions[A, B](value: List[Option[A]]) {
    def composedMap(f: A => B) = {
      composeFunctor(Functor[List], Functor[Option]).map(value)(f)
    }
  }

  "Chapter 5 extra" should "use compose functor" in {
    val listOfOptions: List[Option[Int]] = List(Option(3))
    val maybeStrings: List[Option[String]] = listOfOptions.composedMap(_.toString + " lol")
    assert(maybeStrings == List(Option("3 lol")))
  }


}
