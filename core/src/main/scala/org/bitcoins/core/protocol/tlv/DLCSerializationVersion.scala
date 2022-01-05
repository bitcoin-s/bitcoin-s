package org.bitcoins.core.protocol.tlv

/** We have various binary serializations in our codebase currently.
  * This is a product of trying to release a DLC wallet before the
  * spec was finalized. Some of the binary level serialization for DLCs
  * has changed since we initiallly deployed wallets.
  */
sealed trait DLCSerializationVersion

object DLCSerializationVersion {

  /** This format existed in our wallet before we merged support for this PR
    * on the DLC spec repo. See the diff below
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/pull/144]]
    */
  case object Alpha extends DLCSerializationVersion

  /** This represents binary serialization for the case where we have
    * included support for 144, but not included support for 163 yet
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/pull/144]]
    */
  case object Beta extends DLCSerializationVersion
}
