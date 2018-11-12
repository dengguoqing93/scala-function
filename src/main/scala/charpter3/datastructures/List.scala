package charpter3.datastructures

/**
  * 单向链表
  *
  * @author DGQ
  * @since 2018/11/11
  */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }
  }

  def product(ds: List[Double]): Double = {
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(head, tail) => head * product(tail)
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }
  }

  def setHead[A](list: List[A], head: A): List[A] = {
    list match {
      case Nil => Cons(head, Nil)
      case Cons(_, tail) => Cons(head, tail)
    }
  }
}