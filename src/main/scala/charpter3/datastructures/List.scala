package charpter3.datastructures

import scala.annotation.tailrec

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

  def add1(list: List[Int]): List[Int] = {
    foldLeft(list, Nil: List[Int])((t, h) => Cons(h + 1, t))
  }

  def double2String(list: List[Double]): List[String] = {
    foldLeft(list, Nil: List[String])((t, h) => Cons(h.toString, t))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case Cons(head, tail) => Cons(f(head), map(tail)(f))
    }
  }

  def map1[A, B](as: List[A])(f: A => B): List[B] = {
    foldRightViaFoldLeft(as, Nil: List[B])((h, t) => Cons(f(h), t))
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

  /*
  The discussion about `map` also applies here.
  */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  def filter_1[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRightViaFoldLeft(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    def go(l: List[A]): Unit = {
      l match {
        case Nil => ()
        case Cons(h, t) => if (f(h)) buf += h; go(t)
      }
    }

    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
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

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = {
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }
  }

  def addPairwise1[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = {
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), addPairwise1(t1, t2)(f))
    }
  }

  /*
  This function is usually called `zipWith`. The discussion about stack usage from the explanation of `map` also
  applies here. By putting the `f` in the second argument list, Scala can infer its type from the previous argument list.
  */
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }
  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
}

object ListTest extends App {
  val listInt = List(1, 2, 3, 4, 5, 6)
  println(List.sum3(listInt))
  val listDouble = List(1.0, 2.0, 3.0)
  val map = List.map1(listDouble)(x => x * 3);
  println(map)
}