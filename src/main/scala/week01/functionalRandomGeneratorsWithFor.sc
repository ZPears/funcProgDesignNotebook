// Are for expressions tied to collections?

// NO! All that is needed are implementations of map, flatMap and withFilter.

// an example using random generators...

trait Generator[+T] {
  self => // an alias for "this"
  def generate: T

  def map[U](f: T => U): Generator[U] = new Generator[U] {
    def generate = f(self.generate)
  }

  def flatMap[U](f: T => Generator[U]): Generator[U] = new Generator[U] {
    def generate = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt()
}

integers.generate

val booleans = new Generator[Boolean] {
  def generate = integers.generate > 0
}

val pairs = new Generator[(Int, Int)] {
  def generate = (integers.generate, integers.generate)
}

// some functions using generator...
def single[T](x: T): Generator[T] = new Generator[T] {
  def generate = x
}

def choose(lo: Int, hi: Int): Generator[Int] =
  for (x <- integers) yield lo + x % (hi - lo)

def oneOf[T](xs: T*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(idx)

choose(1,3).generate

val thing: Generator[Int] = integers.map(x => 1 + x % 2)

thing.generate
thing.generate

def lists: Generator[List[Int]] = for {
  isEmpty <- booleans
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def emptyLists = single(Nil)

def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail

lists.generate
lists.generate
lists.generate
lists.generate
lists.generate
lists.generate

// random generator for trees
trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def leafs: Generator[Leaf] = for {
  x <- integers
} yield Leaf(x)

def inners: Generator[Inner] = for {
  x <- trees
  y <- trees
} yield Inner(x,y)

def trees: Generator[Tree] = for {
  isLeaf <- booleans
  tree <- if (isLeaf) leafs else inners
} yield tree

trees.generate
trees.generate
trees.generate

def test[T](g: Generator[T], numTimes: Int = 100)
           (test: T => Boolean): Unit = {
  for (i <- 0 until numTimes) {
    val value = g.generate
    assert(test(value), "test failed for " + value)
  }
  println("passed " + numTimes + " tests")
}

test(integers)(x => x < 1000000)