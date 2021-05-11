package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    {
      val (n, rng1) = rng.nextInt
      if(n<0) {
        return (-(n+1), rng1)
      }
      (n, rng1)
    }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng1) = nonNegativeInt(rng)

    (n/(Int.MaxValue.toDouble+1), rng1)
    //(if (n<0) (-(n+1)/Int.MaxValue.toDouble, rng1) else (n/Int.MaxValue.toDouble, rng1))
  }
  val doubleviaMap: Rand[Double] = {
    map(nonNegativeInt)(_/(Int.MaxValue.toDouble+1))
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((n,d), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (n, rng1) = nonNegativeInt(rng)
    val (d, rng2) = double(rng1)
    ((d,n), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1,d2,d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, i: Int, rng_new: RNG): (List[Int], RNG) ={
      if(i<count) {
        val (n, rng_newer) = rng_new.nextInt
        val (next_value, rng_newest) = go(count, i+1, rng_newer)
        (n :: next_value, rng_newest)
      }
      else{
        (List(), rng_new)
      }
    }
    val (mylist, myrng) =  go(count,0, rng)
    (mylist, myrng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a,b), rng2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {

  }
  def sequenceViaFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f,acc) => map2(f,acc)(_ :: _))
  }
  def intsViaSequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count))(int)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a,b)))
  }
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
