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


  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case _ => Cons(h, l)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, y) => 1 + y)

  def sumLeft(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productLeft(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def lenghtLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((y, _) => 1 + y)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]()) ((x, y) => Cons(y, x))

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }


  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def append_1[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(l1, l2)((x, y) => Cons(y, x))

  def append_2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

//  TRACE FOR concat with foldRight:
//
//  concat(List(List(1,2,3), List(4,5,6)))
//  foldRight(List(List(1,2,3), List(4,5,6)), Nil)(append)
//  append(List(1,2,3), foldRight(List(List(4,5,6)), Nil)(append))
//  append(List(1,2,3), append(List(4,5,6), foldRight(Nil, Nil)(append)))
//  append(List(1,2,3), append(List(4,5,6), Nil)(append))
//  append(List(1,2,3), Cons(4, append(List(5,6), Nil)))
//  append(List(1,2,3), Cons(4, Cons(5, append(List(6), Nil))))
//  append(List(1,2,3), Cons(4, Cons(5, Cons(6, append(Nil, Nil)))))
//  append(List(1,2,3), Cons(4, Cons(5, Cons(6, Nil))))
//  Cons(1, append(List(2,3), Cons(4, Cons(5, Cons(6, Nil)))))
//  Cons(1, Cons(2, append(List(3), Cons(4, Cons(5, Cons(6, Nil))))))
//  Cons(1, Cons(2, Cons(3, append(Nil, Cons(4, Cons(5, Cons(6, Nil)))))))
//  Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))

  def add1(l: List[Int]): List[Int] = foldLeft(l, Nil:List[Int]) ((x, y) => Cons(y+1, x))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String]) ((h, t) => Cons(h.toString, t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldLeft(l, Nil:List[B])((t, h) => Cons(f(h), t))

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filter_1[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((x) => if(f(x)) List(x) else Nil)

  def sumList[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = {
    if(length(l1) != length(l2)) sys.error("Lists size are not compatible.")

    l1 match {
      case Nil => Nil
      case Cons(h1,t1) =>
        l2 match {
          case Nil => Nil
          case Cons(h2, t2) => Cons(f(h1, h2), sumList(t1, t2)(f))
        }
    }
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    @tailrec
    def go(sup1: List[A], sub1: List[A], matches: List[A]): List[A] = {
      (sup1, sub1) match {
        case (Nil, _) | (_, Nil) => matches
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => go(t1, t2, append(matches, List(h2)))
        case _ => Nil
      }
    }

    @tailrec
    def goSup(sup1: List[A], sub1: List[A], matches: List[A]): Boolean = {
      (sup1, sub1) match {
        case (Nil, _) | (_, Nil) => sub == Nil
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 && go(t1, t2, List(h2)) == sub => true
        case (Cons(_, t1), _) => goSup(t1, sub, Nil)
      }
    }

    goSup(sup, sub, Nil)
  }

}

