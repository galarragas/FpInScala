package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case null => 0
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case null => 0
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }


  def depth[A](tree: Tree[A]): Int = tree match {
    case null => 0
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }


  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case null => null
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(fLeaves: A => B)(fNull: B)(fBranches: (B,B) => B) : B = tree match {
    case Leaf(a) => fLeaves(a)
    case null => fNull
    case Branch(left, right) => fBranches(fold(left)(fLeaves)(fNull)(fBranches), fold(right)(fLeaves)(fNull)(fBranches))
  }

  def sizeWithFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(0)(1 + _ + _)

  def maxWithFold(tree: Tree[Int]): Int = fold(tree)(a => a)(0)(_ max _)

  def depthWithFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)(0)((a: Int, b: Int) => 1 + (a max b))

  def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](tree)(a => Leaf(f(a)))(null)(Branch(_,_))
}