//EXAMPLE: Stream for all integers

def from(n: Int): Stream[Int] = n #:: from(n+1)

// works because stream cons operator #:: is lazy with its right operands

// all natural numbers...
val naturals = from(0)

// all multiples of 4...
naturals map (_ * 4)

// Implementing the Sieve of Eratosthenes to calculate prime numbers...

def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess:Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

def isGoodEnough(guess: Double, x: Double): Boolean =
  math.abs( ( guess * guess - x) / x) < 0.0001

sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList