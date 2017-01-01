// Structural Induction applies for any tree structure.

// General principle is the following:
// To prove a property P(t) for all trees t of a certain type,

// 1. Show that P(l) holds for all leaves l of a tree,
// 2. FOr each type of internal node t with subtrees s1, ..., sn, show that
// p(s1) ... p(sn) implies P(t).

// EXAMPLE WITH INT SETS:

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem) NonEmpty(elem, left incl x, right)
    else if (x > elem) NonEmpty(elem, left, right incl x)
    else this
  }

}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
}