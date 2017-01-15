import week02.waterPouring._

val problem = new Glasses(Vector(4,9, 19))

problem.pourPathSets.take(3).toList.foreach { x =>
  println("---New Round---")
  x.foreach(println)
}

println("---SOLUTIONS---")
problem.solutions(3).foreach(println)