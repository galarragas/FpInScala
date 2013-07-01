package fpinscala.errorhandling

import scala.{Option, Some}
import fpinscala.errorhandling.Some

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  /*
  Of course, we can also implement `flatMap` with explicit pattern matching.
  */
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  
  def orElse[B>:A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob
  /*
  Again, we can implement this with explicit pattern matching. 
  */
  def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }
  
  def filter(f: A => Boolean): Option[A] =   {
//    def extractValue: (Boolean => Option[A]) = (bool => if(bool) this else None)
    map(f) map (if(_) this else None) getOrElse(None)
  }
  /*
  This can also be defined in terms of `flatMap`.
  */
  def filter_1(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val x: Int = throw new Exception("fail!")
    try {
      val y = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  import java.util.regex._
  
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] = 
    pattern(pat) map (p => (s: String) => p.matcher(s).matches) // The details of this API don't matter too much, but `p.matcher(s).matches` will check if the string `s` matches the pattern `p`.

  def mkMatcher_1(pat: String): Option[String => Boolean] = 
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)
  
  def doesMatch(pat: String, s: String): Option[Boolean] = 
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat) flatMap (f => 
    mkMatcher(pat2) map     (g => 
    f(s) && g(s)))

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).map(media => xs.map(value => math.pow(value - media, 2))).flatMap(mean(_))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(y =>f(x, y)))

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = map2(mkMatcher(pat1), mkMatcher(pat2))(_(s) && _(s))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldLeft(Some(List()): Option[List[A]])(
    (acc, x) => acc.flatMap(l => x.map(elem => elem :: l))
  )

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}