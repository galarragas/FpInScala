package fpinscala.datastructures

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1,2,3)
  val total = sum(example)

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

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(l: List[Int]) = 
    foldRight(l, 0.0)(_ + _)
  
  def product2(l: List[Double]) = 
    foldRight(l, 1.0)(_ * _)


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def drop[A](l: List[A], n: Int): List[A] = if((n == 0) || l == Nil) l else drop(tail(l), n-1)

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head,tail) => if(f(head)) dropWhile(tail)(f) else Cons(head, dropWhile(tail)(f))
  }

  def setHead[A](l: List[A])(h: A): List[A] = Cons(h, tail(l))

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,b) => 1 + b)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => append(reverse(tail), Cons(head, Nil))
  }

  def foldLeft_r[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((a,b) => f(b, a))

  def append_fold[A](first: List[A], second: List[A]) : List[A] = foldRight(first, second)(Cons(_, _))

  def concatListOfList[A](deepList: List[List[A]]): List[A] = foldLeft(deepList, List[A]())(append)

  def addOneToList(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case Cons(head, tail) => Cons(head + 1, addOneToList(tail))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(head, tail) => Cons(f(head), map(tail)(f))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => if(f(head)) Cons(head, filter(tail)(f)) else filter(tail)(f)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = foldLeft(l, List[B]())((b, a) => append(b, f(a)))

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if(f(a)) Cons(a, Nil) else Nil)

  def composeLists[A,B, C](left: List[A], right: List[B])(f: (A, B) => C): List[C] = (left, right) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(leftHead, leftTail), Cons(rightHead, rightTail)) => Cons(f(leftHead, rightHead), composeLists(leftTail, rightTail)(f))
  }

  def hasSubsequence[A](master: List[A], sublist: List[A]): Boolean = {
    def startsWith[A](list: List[A], sub: List[A]): Boolean = if(length(list) < length(sub)) false else foldLeft(composeLists(list, sub)(_ == _), true)(_ && _)

    if(sublist == Nil) true
    else
      master match {
        case Nil => false
        case _ => startsWith(master, sublist) || startsWith(tail(master), sublist)
      }
  }
}