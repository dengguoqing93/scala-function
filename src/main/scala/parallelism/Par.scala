package parallelism


import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit
import language.implicitConversions

/**
  * $DESCRIPTION
  *
  * @author DGQ
  * @since 2018/11/20
  */
object Par {
  type Par[A] = ExecutorService => Future[A]


  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = {
    (es: ExecutorService) => UnitFuture(a)
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  }

  def fork[A](a: => Par[A]): Par[A] ={
    es => es.submit(new Callable[A] {
     def call = a(es).get()
    })
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
    map2(pa, unit(()))((a, _) => f(a))
  }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
    fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] = {
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }
  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

}
