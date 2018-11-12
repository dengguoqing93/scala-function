package chapter2

import scala.annotation.tailrec

/**
  * $DESCRIPTION
  *
  * @author DGQ
  * @since 2018/11/11
  */
object Fibonacci {

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    }

    loop(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(fib(6))
  }
}
