package solutions.chapter3

import cats._
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Write a Functor for the following binary tree data type. Verify that the code
  * works as expected on instances of Branch and Leaf:
  * sealed trait Tree[+A]
  * final case class Branch[A](left: Tree[A], right: Tree[A])
  * extends Tree[A]
  * final case class Leaf[A](value: A) extends Tree[A]
  */
sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]
class Exercise_3_5_4Test extends FlatSpec with Matchers {
  /**
    * Functor type class instance for Tree
    * // Note that because some standard types in scala defines map, cats provides fmap as an alias to the Cat's own map
    */
  implicit object treeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(treeValue) => Leaf(f(treeValue))
    }
  }

  "Chapter three, exercise 3.5.4" should "use functor instance for tree by using Functor's Apply" in {
    // (2) turns (4)
    assert(Functor[Tree].fmap(Leaf(2))(_ * 2) == Leaf(4))

    /**
      *  maps
      *
      *    (*)
      *    / \
      *  (2) (3)
      *
      *  into
      *
      *    (*)
      *    / \
      *  (4) (6)
      */
    assert(Functor[Tree].fmap(Branch(Leaf(2), Leaf(3)))(_ * 2) == Branch(Leaf(4), Leaf(6)))


    /**
      *  maps
      *
      *    (*)
      *    / \
      *  (2) (*)
      *      / \
      *   (3) (6)
      *  into
      *
      *    (*)
      *    / \
      *  (4) (*)
      *      / \
      *   (6) (12)
      */
    assert(Functor[Tree].fmap(Branch(Leaf(2), Branch(Leaf(3), Leaf(6))))(_ * 2) == Branch(Leaf(4), Branch(Leaf(6), Leaf(12))))
  }

  "Chapter three, exercise 3.5.4" should "use functor instance for tree by using fmap syntax" in {
    // Explicit type annotation with Tree[Int] is necessary to get the fmap method, since we defined the functor
    // instance for Tree and not for Leaf

    // (2) turns (4)
    assert((Leaf(2):Tree[Int]).fmap(_ * 2) == Leaf(4))

    /**
      *  maps
      *
      *    (*)
      *    / \
      *  (2) (3)
      *
      *  into
      *
      *    (*)
      *    / \
      *  (4) (6)
      */
    assert((Branch(Leaf(2), Leaf(3)):Tree[Int]).map(_ * 2) == Branch(Leaf(4), Leaf(6)))

    /**
      *  maps
      *
      *    (*)
      *    / \
      *  (2) (*)
      *      / \
      *   (3) (6)
      *  into
      *
      *    (*)
      *    / \
      *  (4) (*)
      *      / \
      *   (6) (12)
      */
    assert((Branch(Leaf(2), Branch(Leaf(3), Leaf(6))):Tree[Int]).map(_ * 2) == Branch(Leaf(4), Branch(Leaf(6), Leaf(12))))
  }

  "Chapter three, exercise 3.5.4" should "use smart objects for creating instances of Tree needed for mapping" in {
    // alternative to type annotation is to provide smart constructors. Smart constructors would ensure that Leaf for
    // instance is returned as Tree, removing the need for explicit type annotating to Tree.

    object Tree {
      def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
        Branch(left, right)
      def leaf[A](value: A): Tree[A] =
        Leaf(value)
    }

    assert(Tree.leaf(100).map(_ * 2) == Tree.leaf(200))
    assert(Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2) == Tree.branch(Tree.leaf(20), Tree.leaf(40)))
  }

}
