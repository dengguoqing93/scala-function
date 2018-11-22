package state

/**
  * $DESCRIPTION
  *
  * @author DGQ
  * @since 2018/11/19
  */
trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

case class Simple(seed: Long) extends RNG {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1) // We pass the new state along
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = Simple(
      newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16)
      .toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }


  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r1) = double(r)
    ((i, d), r1)
  }

  def doubleInt1(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r1) = r.nextInt
    ((d, i), r1)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (List(), rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }
  }

  // A tail-recursive solution
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (count == 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }
    }

    go(count, rng, List())
  }
}


import State._

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = {
    State(s => (a, s))
  }

  // The idiomatic solution is expressed via foldRight
  def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
  }

  // This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) = {
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }
    }

    State((s: S) => go(s, sas, List()))
  }

  // We can also write the loop using a left fold. This is tail recursive like the
  // previous solution, but it reverses the list _before_ folding it instead of after.
  // You might think that this is slower than the `foldRight` solution since it
  // walks over the list twice, but it's actually faster! The `foldRight` solution
  // technically has to also walk the list twice, since it has to unravel the call
  // stack, not being tail recursive. And the call stack will be as tall as the list
  // is long.
  def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))
  }

  def modify[S](f: S => S): State[S, Unit] = {
    for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = {
    (i: Input) =>
      (s: Machine) =>
        (i, s) match {
          case (_, Machine(_, 0, _)) => s
          case (Coin, Machine(false, _, _)) => s
          case (Turn, Machine(true, _, _)) => s
          case (Coin, Machine(true, candy, coin)) =>
            Machine(false, candy, coin + 1)
          case (Turn, Machine(false, candy, coin)) =>
            Machine(true, candy - 1, coin)
        }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)
  }
}