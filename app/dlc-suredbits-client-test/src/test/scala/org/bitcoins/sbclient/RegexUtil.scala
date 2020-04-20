package org.bitcoins.sbclient

import scala.util.matching.Regex

object RegexUtil {

  /** Given a finite collection of case objects or strings, generates a Regex
    * that matches its elements, insensitive to case.
    *
    * Example:
    * case object A
    * case object BC
    * case object DEF
    *
    * Util.noCaseOrRegex(Vector(A, BC, DEF)) == "(?i)(?:DEF|BC|A)".r
    */
  def noCaseOrRegex[T](all: Seq[T]): Regex = {
    val orStr = all
      .sortBy(_.toString.length)(Ordering.Int.reverse)
      .foldLeft("") {
        case (rx, t) =>
          rx + t + "|"
      }
      .init

    noCaseRegex(orStr)
  }

  /** Turns a String into a case insensitive Regex */
  def noCaseRegex(str: String): Regex = {
    s"""(?i)(?:$str)""".r
  }

  /** Matches any String of any length */
  val anyString: Regex = ".*".r
}
