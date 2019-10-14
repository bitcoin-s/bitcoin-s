package org.bitcoins.core.compat

import scala.util.Try
import scala.util.Success
import scala.util.Failure

/** This is an implementation of (parts of)
  * `scala.util.Either`, compatible with Scala 2.11,
  * 2.12 and 2.13. It is in large parts cribbed from
  * the Scala 2.12 standard library.
  */
sealed private[bitcoins] trait CompatEither[+A, +B] {

  protected val underlying: Either[A, B]

  /** The given function is applied if this is a `Right`.
    *
    *  {{{
    *  Right(12).map(x => "flower") // Result: Right("flower")
    *  Left(12).map(x => "flower")  // Result: Left(12)
    *  }}}
    */
  def map[B1](f: B => B1): CompatEither[A, B1] = underlying match {
    case Right(b) => CompatRight(f(b))
    case _        => this.asInstanceOf[CompatEither[A, B1]]
  }

  /** Binds the given function across `Right`.
    *
    *  @param f The function to bind across `Right`.
    */
  def flatMap[A1 >: A, B1](f: B => CompatEither[A1, B1]): CompatEither[A1, B1] =
    underlying match {
      case Right(b) =>
        f(b) match {
          case CompatLeft(value)  => CompatLeft(value)
          case CompatRight(value) => CompatRight(value)

        }
      case Left(l) => CompatLeft(l)
    }

  def toTry(implicit ev: A <:< Throwable): Try[B] = underlying match {
    case Right(b) => Success(b)
    case Left(a)  => Failure(a)
  }

  /** Applies `fa` if this is a `Left` or `fb` if this is a `Right`.
    *
    *  @example {{{
    *  val result = util.Try("42".toInt).toEither
    *  result.fold(
    *    e => s"Operation failed with $e",
    *    v => s"Operation produced value: $v"
    *  )
    *  }}}
    *
    *  @param fa the function to apply if this is a `Left`
    *  @param fb the function to apply if this is a `Right`
    *  @return the results of applying the function
    */
  def fold[C](fa: A => C, fb: B => C): C = underlying match {
    case Right(b) => fb(b)
    case Left(a)  => fa(a)
  }
}

object CompatEither {

  /** Converts the given `scala.util.Either` to a `CompatEither` */
  def apply[A, B](either: Either[A, B]): CompatEither[A, B] = either match {
    case Left(value)  => CompatLeft(value)
    case Right(value) => CompatRight(value)
  }

}

/** Analogous to `scala.util.Left` */
case class CompatLeft[A, B](value: A) extends CompatEither[A, B] {
  val underlying = scala.util.Left[A, B](value)

}

/** Analogous to `scala.util.Right` */
case class CompatRight[A, B](value: B) extends CompatEither[A, B] {
  val underlying = scala.util.Right[A, B](value)
}
