package org.bitcoins.crypto

/** Meant to provide a simple trait that
  * masks the default to string for sensitive classes */
trait MaskedToString {
  final override def toString: String = {
    s"Masked(${getClass.getSimpleName})"
  }

  /** Returns the real value of a sensitive string
    * This should be considered unsafe in the sense
    * that this information is sensitive and could cause
    * loss of funds if used anywhere things are persisted like logs
    *
    * */
  def toStringSensitive: String
}
