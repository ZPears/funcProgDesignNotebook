import scala.util.control.NonFatal

// A MONAD is a parametric type M[T] with two operations, flatMap and unit,
// that have to satisfy some laws.

trait M[T] {
  def flatMap[U](f: T => M[U]): M[U]
}

def unit[T](x: T): M[T]

// map can be defined for every monad as a combo of flatMap and unit:

// m map f == m flatMap( x => unit(f(x)) )
//         == m flatMap( f andThen unit )

// MONAD QUALIFICATIONS:

// Associativity:
// m flatMap f flatMap g == m flatMap ( x => f(x) flatMap g )

// Left Unit:
// unit(x) flatMap f == f(x)

// Right Unit:
// m flatMap unit = m

// EXAMPLE: OPTION

abstract class Option[+T] {

  def flatMap[U](f: T => Option[U]): Option[U] = this match {
    case Some(x) => f(x)
    case None => None
  }

}

// LEFT LAW: Show that Some(x) flatMap f == f(x)
// Some(x) flatMap f
// == Some(x) match case Some(x) => f(x)
// f(x) == f(x)

// RIGHT LAW: Show that opt flatMap Some == opt
// opt flatMap Some
// ... case Some(x) => f(x)
// case Some(x) => Some(x)
// Some(x) == Some(x)

// MONAD EXAMPLE: TRY


case class Success[T](x: T)         extends Try[T]
case class Failure(ex: Exception)   extends Try[Nothing]

object Try {
  def apply[T](expr: => T): Try[T] =
    try Success(expr)
    catch {
      case NonFatal(ex: Exception) => Failure(ex)
    }
}

abstract class Try[+T] {

  def flatMap[U](f: T => Try[U]): Try[U] = this match {
    case Success(x) => try f(x) catch { case NonFatal(ex: Exception) => Failure(ex) }
    case fail: Failure => fail
  }

  def map[U](f: T => U): Try[U] = this match {
    case Success(x) => Try(f(x))
    case fail: Failure => fail
  }

}