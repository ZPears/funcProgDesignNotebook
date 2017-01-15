package week02.waterPouring

/**
  * Created by zach on 1/14/17.
  */
class Glasses(capacities: Vector[Int]) {

  type GlassIdx = Int
  type State = Vector[Int]
  val initialState = capacities map (x => 0)

  trait Move { def change(state: State): State }

  case class Empty(glass: GlassIdx) extends Move {
    def change(state: State) = state updated (glass, 0)
  }
  case class Fill(glass: GlassIdx) extends Move {
    def change(state: State) = state updated (glass, capacities(glass))
  }
  case class Pour(from: GlassIdx, to: GlassIdx) extends Move {
    def change(state: State): State = {
      val amount = state(from) min (capacities(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  val glassIndices = capacities.indices

  val moves =
    (for (g <- glassIndices) yield Empty(g)) ++
    (for (g <- glassIndices) yield Fill(g)) ++
    (for (from <- glassIndices; to <- glassIndices if from != to) yield Pour(from, to))

  class PourPath(history: List[Move], val endState: State) {
    def extend(move: Move) = new PourPath(move :: history, move change endState)
    // this is equivalent to (history foldRight initialState) (_ change _)
    // since it builds a stack as it goes along the list from last move at the head to first move at the tail
    // then folds them all back starting from the rightmost (last) element
    private def trackState(xs: List[Move]): State = xs match {
      case Nil => initialState
      case move :: moves => move change trackState(moves)
    }

    override def toString = (history.reverse mkString " ") + "--> " + endState

  }

  def from(paths: Set[PourPath], explored: Set[State]): Stream[Set[PourPath]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val initialPath = new PourPath(Nil, initialState)
  val pourPathSets = from(Set(initialPath), Set(initialState))

  def solutions(target: Int): Stream[PourPath] = {
    for {
      pathSet <- pourPathSets
      path <- pathSet
      if path.endState contains target
    } yield path
  }

}
