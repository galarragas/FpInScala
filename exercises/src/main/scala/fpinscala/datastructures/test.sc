import fpinscala.applicative.Applicative

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil => Some(List())
  case x::xs => f(x) match {
    case None => None
    case Some(fx) => traverse(xs)(f).map( fx :: _ )
  }
}

def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil => Some(List())
  case x::xs =>  f(x).map(fx => fx :: traverse2(xs)(f).getOrElse(List()) )
}

def traverse3[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(List()))(
  (x, acc) => acc.flatMap( f(x) :: _)
)

def noneIfEven(value: Int) = if((value % 2) == 0) None else Some(value)


val resultNone = traverse(List(1,2,3,4))(noneIfEven)
val resultSome = traverse(List(1,3))(noneIfEven)

val resultNone2 = traverse2(List(1,2,3,4))(noneIfEven)
val resultSome2 = traverse2(List(1,3))(noneIfEven)