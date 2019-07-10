package org.bitcoins.core.hd

/** The address types covered by BIP44, BIP49 and BIP84 */
sealed abstract class AddressType

object AddressType {

  /** Uses BIP84 address derivation, gives bech32 address (`bc1...`) */
  final case object SegWit extends AddressType

  /** Uses BIP49 address derivation, gives SegWit addresses wrapped
    * in P2SH addresses (`3...`)
    */
  final case object NestedSegWit extends AddressType

  /** Uses BIP44 address derivation (`1...`) */
  final case object Legacy extends AddressType
}
