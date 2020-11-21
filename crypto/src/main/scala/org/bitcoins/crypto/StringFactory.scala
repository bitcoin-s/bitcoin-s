package org.bitcoins.crypto

import scala.util.Try

/** A common factory trait that can be re-used to deserialize a string to a type t */
trait StringFactory[+T] {

  /** Tries to parse a string to type t, throws an exception if fails */
  def fromString(string: String): T

  /** Treis to parse a string to type t, returns None if failure */
  def fromStringOpt(string: String): Option[T] = {
    fromStringT(string).toOption
  }

  /** Tries to parse string to type t, returns [[scala.util.Failure]] if the fails */
  def fromStringT(string: String): Try[T] = {
    Try(fromString(string))
  }
}
