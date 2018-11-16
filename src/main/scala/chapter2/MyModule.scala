package chapter2

import scala.annotation.tailrec

/**
  * $DESCRIPTION
  *
  * @author DGQ
  * @since 2018/11/11
  */
object MyModule {
  def abs(n: Int) = if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  /**
    * 阶乘
    *
    * @param n
    * @return
    */
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "the %s of %d is %d."
    msg.format(name, n, f(n))
  }

  /**
    * 斐波那契数列
    *
    * @param n 数列的位置
    * @return 第n个位置的数
    */
  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    }

    loop(n, 0, 1)
  }

  /**
    * 查询数组中字符串第一次出现的索引值
    *
    * @param ss  字符串数组
    * @param key 查询字符串
    * @return 索引值
    */
  def findFirst(ss: Array[String], key: String): Int = {
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }

    loop(0)
  }

  /**
    * 一个泛型函数，查询某值是否在数组中
    *
    * @param as 数据值
    * @param p  待查询值
    * @tparam A 数据类型
    * @return 数据索引，若不存在数组中，返回-1
    */
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n == as.length) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  /**
    * 科里化
    *
    * @param f 科里化函数
    * @tparam A A
    * @tparam B B
    * @tparam C C
    * @return C
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def unCurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
  val t = List
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    val t = List(1,2,3)
    println(t.size)
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }
}
