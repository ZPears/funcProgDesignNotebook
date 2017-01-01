// Iterable

// Seq, Set, Map

// Seq: IndexedSeq, LinearSeq

// IndexedSeq: Vector, Array, String
// LinearSeq: List

// ALL COLLECTIONS IMPLEMENT MAP, FLATMAP, FILTER, FOLD

/*abstract class List[+T] {

  def map[U](f: T => U): List[U] = this match {
    case x :: xs => f(x) :: xs.map(f)
    case Nil => Nil
  }

  def flatMap[U](f: T => List[U]): List[U] = this match {
    case x :: xs => f(x) ++ flastMap(f)
    case Nil => Nil
  }

  def filter(p: T => Boolean): List[T] = this match {
    case x :: xs =>
      if (p(x)) x :: xs.filter(p) else xs.filter(p)
    case Nil => Nil
  }
}*/

def isPrime(n: Int): Boolean = {
  if (n == 2 || n == 3) { true }
  else if (n % 2 == 0 || n % 3 == 0) {false}
  else {
    var i = 5
    var w = 2
    while (i * i <= n) {
      if (n % i == 0) {return false}
      else {i += w; w = 6 - w}
    }
    true
  }


}

(1 until 10) flatMap (i =>
  (1 until 10) filter (j => isPrime(i + j)) map
    (j => (i,j)))

val primes = for {
  i <- 1 until 10
  j <- 1 until i
  if isPrime(i + j)
} yield (i,j)

primes.foreach(println)