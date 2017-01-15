import scala.collection.immutable.Range.Inclusive

// Streams are similar to lists, but their tail is evaluated only on demand.

val xs = Stream.cons(1, Stream.cons(2, Stream.empty))

(1 to 1000).toStream

xs.tail

// stream conceptual example
object PrimeFinder {
  def nthPrime(r: Range, n: Int): Int = {

    def primeCounter(guess: Int, remaining: Int): Int = {

      if (remaining == 1 & isPrime(guess)) guess
      else if (guess == r.end) throw new IllegalArgumentException(s"There are < $n prime numbers in $r")
      else if (isPrime(guess)) primeCounter(guess+1, remaining-1)
      else primeCounter(guess+1, remaining)

    }

    primeCounter(r.start, n)
  }

  private def isPrime(x: Int): Boolean = {
    if (x <= 1) false
    else if (x == 2) true
    else !(2 until x).exists(i => x % i == 0)
  }
}

PrimeFinder.nthPrime(1 to 100, 5)

// example of stream vs. list from first principles
def streamRange(lo: Int, hi: Int): Stream[Int] =
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))

def listRange(lo: Int, hi: Int): List[Int] =
  if (lo >= hi) Nil
  else lo :: listRange(lo + 1, hi)

// OPERATOR FOR PRODUCING STREAMS
val l: List[Int] = 1 :: List(2,3) //Produces List
val s = x #:: xs // Produces Stream