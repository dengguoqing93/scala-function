package charpter3.datastructures

/**
  * $DESCRIPTION
  *
  * @author DGQ
  * @since 2018/11/14
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left) max (maximum(right))
    }
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(a => 1)(1 + _ + _)
  }

  def maximumViaFold(t: Tree[Int]): Int = {
    fold(t)(a => a)(_ max _)
  }

  def depthViaFold[A](t: Tree[A]): Int = {
    fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))
  }

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  }
}
