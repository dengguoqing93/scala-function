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
      case Cons(head, tail) => head * product(tail)
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }
  }

  def foldLeft[T, U](as: List[T], z: U)(f: (U, T) => U): U = {
    as match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }
  }

  def length1[T](ns: List[T]): Int = {
    foldLeft(ns, 0)((acc, _) => acc + 1)
  }

  def reverse[T](list: List[T]): List[T] = {
    foldLeft(list, List[T]())((acc, h) => Cons(h, acc))
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)((x, y) => x * y)

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

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

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail)(f)
      case _ => l
    }
  }

  def append[A](list1: List[A], list2: List[A]): List[A] = {
    list1 match {
      case Nil => list2
      case Cons(head, tail) => Cons(head, append(tail, list2))
    }
  }

  def append1[A](list1: List[A], list2: List[A]): List[A] = {
    foldLeft(list1, list2)((acc, res) => Cons(res, acc))
  }

  def append2[A](list1: List[A], list2: List[A]): List[A] = {
    foldRight(list1, list2)((acc, res) => Cons(acc, res))
  }

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((b, a) => f(a, b))
  }

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def go(cur: List[A]): List[A] = {
      cur match {
        case Nil => sys.error("init of empty list")
        case Cons(_, Nil) => List(buf.toList: _*)
        case Cons(h, t) => buf += h; go(t)
      }
    }

    go(l)
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

}

object ListTest extends App {
  val listInt = List(1, 2, 3, 4, 5, 6)
  println(List.sum3(listInt))
  val listDouble = List(1.0, 2.0, 3.0)
  println(List.product3(listDouble))
}