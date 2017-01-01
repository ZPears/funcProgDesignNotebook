// for expressions can be thought of kind of like a query language...

case class Book(title: String, authors: List[String])

val books: Set[Book] = Set(
  Book(title="Attack On Titan", authors=List("Hajime Isayaka")),
  Book(title="REAL Ultimate Power", authors=List("Robert Hamburger")),
  Book(title="Design Patterns", authors=List("Gamma", "Helm", "Johnson", "Vlissides")),
  Book(title="Ninja Manual", authors=List("Robert Hamburger"))
)

for (b <- books; a <- b.authors if a startsWith "H") yield b.title

for (b <- books if (b.title indexOf "Ultimate") >= 0) yield b.title

// names of all authors with > 2 books in DB
for {
  b1 <- books
  b2 <- books
  if b1.title < b2.title
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield b1.authors

// map, flatMap and filter can be expressed in terms of for expressions...

def mapFun[T,U](xs: List[T], f: T => U): List[U] = {
  for ( x <- xs ) yield f(x)
}

def flatMapFun[T,U](xs: List[T], f: T => Iterable[U]): List[U] = {
  for {x <- xs
       y <- f(x)
  } yield y
}

def filterFun[T](xs: List[T], p: T => Boolean): List[T] = {
  for { x <- xs if p(x) } yield x
}

// withFilter - a lazy variant of filter e1.withFilter(x => f);

for (b <- books; a <- b.authors if a startsWith "Ha") yield b.title

books.flatMap{ b =>
  for (a <- b.authors if a startsWith "Ha") yield b.title
}

books.flatMap{ b =>
  for (a <- b.authors withFilter (a => a startsWith "Ha")) yield b.title
}

books.flatMap( b => b.authors withFilter (a => a startsWith "Ha") map (y=>y))

