package org.bitcoins.core.wallet.utxo

import org.bitcoins.testkit.util.BitcoinSUnitTest

class AddressTagTest extends BitcoinSUnitTest {

  behavior of "AddressTag"

  it must "read StorageLocationTag from string" in {
    StorageLocationTag.fromString("HotStorage") must be(
      StorageLocationTag.HotStorage)

    StorageLocationTag.fromString("ColdStorage") must be(
      StorageLocationTag.ColdStorage)

    StorageLocationTag.fromString("DeepColdStorage") must be(
      StorageLocationTag.DeepColdStorage)
  }

  it must "read StorageLocationTagName from string" in {
    InternalAddressTagName.fromString("HotStorage") must be(
      StorageLocationTag.HotStorageName)

    InternalAddressTagName.fromString("ColdStorage") must be(
      StorageLocationTag.ColdStorageName)

    InternalAddressTagName.fromString("DeepColdStorage") must be(
      StorageLocationTag.DeepColdStorageName)
  }
}
