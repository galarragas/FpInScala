package fpinscala.laziness.prova

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case None => Stream.empty
    case Some((head, tail)) => if(p(head)) Stream.cons(head, tail.takeWhile(p)) else Stream.empty
  }

  def takeWhileWithFold(p: A => Boolean): Stream[A] = foldRight(Stream.empty: Stream[A])((elem, acc) => { print("."); if (p(elem)) Stream.cons(elem, acc) else Stream.empty[A] } )

  def forAll(p: A => Boolean): Boolean = sys.error("todo")

  def toList: List[A] = foldRight(List.empty: List[A])((elem, accumulator) => elem :: accumulator)

  def take(n: Int): Stream[A] = if(n == 0) Stream.empty else uncons match {
    case None => Stream.empty
    case Some((head, tail)) => Stream.cons(head, tail.take(n-1))
  }

  def asString: String = uncons match {
    case Some((h, t)) => h.toString
    case None => "NONE"
  }

  def takeWithFold(n: Int): Stream[A] = {
    type Accumulator = (Stream[A], Int)
    foldRight((Stream.empty, n): Accumulator)( (elem, accumulator) =>
      {
        print(".")
        if(accumulator._2 > 0) (Stream.cons(elem, accumulator._1), accumulator._2 - 1) else (Stream.empty[A], 0)
      }
    )._1
  }

  def mapOrig[B](f: A => B ): Stream[B] = uncons match {
    case None => Stream.empty[B]
    case Some((head, tail)) => Stream.cons(f(head), tail.mapOrig(f))
  }

  def map[B](f: A => B ): Stream[B] = Stream.unfold(uncons)(_.map(tuple => (f(tuple._1), tuple._2.uncons)))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this)(_.uncons.map ( tuple => (f(tuple._1), tuple._2) ))
}

object Stream {
  def empty[A]: Stream[A] = 
    new Stream[A] { def uncons = None }
  
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = 
    new Stream[A] {
      lazy val uncons = Some((hd, tl)) 
    }
  
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def consts[A](value: A) : Stream[A] = cons(value, consts(value))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  val fibs: Stream[Int] =  {
    def fib(first: Int, second: Int): Stream[Int] = cons(first + second, fib(second, first + second))

    cons(0, cons(1, fib(0, 1)))
  }




  def unfoldUberto[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def unfoldr(state: S): Stream[A] = {
        val next = f(state)
        next match {
          case None => Stream.empty
          case Some(nextState) => cons(nextState._1, unfoldr( nextState._2))
      }
    }

    unfoldr(z)

  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =  f(z) match {
      case None =>  Stream.empty
      case Some(nextState) => cons(nextState._1, unfold(nextState._2)(f))
    }

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = sys.error("todo")

  def zip[A, B](s: Stream[A], s2: Stream[B]) : Stream[(A,B)] = unfold((s, s2))(
  {
    case(st1: Stream[A], st2: Stream[B]) => (st1.uncons, st2.uncons) match {
        case (Some((st1h: A, st1t: Stream[A])), Some((st2h: B, st2t: Stream[B]))) => Some((st1h, st2h), (st1t, st2t))
        case _ => None
    }
  }
  )

}