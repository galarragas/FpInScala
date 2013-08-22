package fpinscala.state.exercises


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We will later define other functions in terms of `nextInt`.
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & // `&` is bitwise AND
                  ((1L << 48) - 1) // `<<` is left binary shift
      ((seed2 >>> 16).asInstanceOf[Int], // `>>>` is right binary shift with zero fill
       simple(seed2))
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

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (_nextInt, nextRNG) = rng.nextInt

    if(Int.MinValue == _nextInt) positiveInt(nextRNG)
    else (Math.abs(_nextInt), nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRng) = positiveInt(rng)

    (nextInt.toDouble/Int.MaxValue.toDouble, nextRng)
  }

  def doubleWithMap(rng: RNG): (Double, RNG) =  map(positiveInt)( _.toDouble/Int.MaxValue.toDouble ) (rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (ni, nr) = rng.nextInt
    val (nd, nnr) = double(nr)
    ((ni, nd), nnr)

  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((ni, nd), nr) = intDouble(rng)
    ((nd, ni), nr)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (nd1, nr1) = double(rng)
    val (nd2, nr2) = double(nr1)
    val (nd3, nn3) = double(nr2)
    ((nd1, nd2, nd3), nn3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0) (List.empty[Int], rng)
    else {
      val (nextInt, nextRng) = int(rng)
      val (nextList, _rng) = ints(count -1)(nextRng)

      (nextInt :: nextList, _rng)
    }
  }

  def intStream(rng: RNG) : (Stream[(Int, RNG)]) = {
    val head = int(rng)
    Stream.cons(head, intStream(head._2) )
  }

  def positiveMax(n: Int): Rand[Int] = map(positiveInt)(_ % n)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rnga) = ra(rng)
    val (b, rngb) = rb(rnga)

    (f(a, b), rngb)
  }

  def intDoubleWithMap(rng: RNG): ((Int,Double), RNG) = map2(int, double)((a, b) => (a, b))(rng)

  def doubleIntWithMap(rng: RNG): ((Double,Int), RNG) = map2(double, int)((a, b) => (a, b))(rng)
  def doubleIntWithMap2(rng: RNG): ((Double,Int), RNG) = map2(int, double)((a, b) => (b, a))(rng)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = sys.error("todo")

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = sys.error("todo")
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State( state => { val baseVal = run(state)
                                                          (f(baseVal._1), baseVal._2)
                                                        }
                                            )

  def map2[B,C](that: State[S, B])(f: (A, B) => C): State[S, C] =
     State( state => {
                  val thisBaseVal : (A, S) = run(state)
                  val thatBaseVal : (B, S) = that.run(thisBaseVal._2)
                  ( f(thisBaseVal._1, thatBaseVal._1), thatBaseVal._2 )
                }
          )

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State( state => {
                  val baseVal = run(state)
                  f(baseVal._1).run(baseVal._2)
            }
        )
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, Int] = {
    def machineEventHandler(currStatus: Machine, input: Input): Machine = currStatus match {
      case Coin => currStatus match {
        case Machine(false, _, _) => currStatus //Ignoring any coin when unlocked
        case Machine(true, 0, _) => currStatus
        case Machine(true, candies, coins) => Machine(false, candies, coins + 1)
      }
      case Turn => currStatus match {
        case Machine(true, _, _) => currStatus
        case Machine(false, candies, coins) => Machine(true, candies -1, coins)
      }
    }

    val endStatus = inputs.foldLeft (Machine(locked = true, candies = 10, coins = 0)) (machineEventHandler(_, _))
  }
}