package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  def length[A](l: List[A]): Int =
    foldRight(l, 1)((_, y) => 1 + y)

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((x,y) => Cons(y,x))

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error('nilllll')
      case Cons(h ,t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error('nilllll')
      case Cons(_,t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    if ( n <= 0) l
    else l match {
      case Nil => sys.error('niodksl')
      case Cons(h, 0) => h
      case Cons(h, t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => sys.error('nilllll')
      case Cons(h,t) => if(f(h)) dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error('nilll')
      case Cons(_, Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs , f(x, z)(f))
    }

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    foldLeft(as, Nil: List[A])((y,x) => f(x,y))

  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x,y) => Cons(x,y))

  def addOne(l: List[Int]):List[Int] =
    foldRight(l,Nil: List[Int])((x,y) => Cons(x+1,y))
  def doubleToString(l : List[Double]):List[String] =
    foldRight(l, Nil: List[String]) ((x,y)=> Cons(x.toString,y))

  def concat[A](l : List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])((x,y) => append(x,y))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((x,y) => Cons(f(x),y))

  def flatmap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil:List[B])((x,y) => append(f(x),y)
  def addPairWise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2, addPairWise(t1,t2))
    }
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A,B) => C): List[C] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }

  def Filter[A,B](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((x,y) => if(f(x)) Cons(x,y) else y)

  def FilterFlat[A,B](l: List[A])(f: A => Boolean): List[A] =
    flatmap(l)(x => if(f(x)) List(x) else Nil))
}
