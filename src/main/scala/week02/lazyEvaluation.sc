// Consider the following:

def expr = {
  val x = { print("x"); 1 }
  lazy val y = { print("y"); 2 }
  def z = { print("z"); 3 }
  z + y + x + z + y + x
}

// what would evaluating expr produce?
val answer = "xzyz"