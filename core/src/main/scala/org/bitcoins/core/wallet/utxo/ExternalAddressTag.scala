package org.bitcoins.core.wallet.utxo

/** Address Tag Names defined outside the library, used for other projects
  * creating there own address tags that aren't supported by bitcoin-s
  */
trait ExternalAddressTagName extends AddressTagName

/** Address Tag Types defined outside the library, used for other projects
  * creating there own address tags that aren't supported by bitcoin-s
  */
trait ExternalAddressTagType extends AddressTagType

/** Address Tags defined outside the library, used for other projects
  * creating there own address tags that aren't supported by bitcoin-s
  */
trait ExternalAddressTag extends AddressTag
