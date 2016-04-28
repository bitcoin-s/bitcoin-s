package org.bitcoins.protocol

import org.bitcoins.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/23/15.
 */
class AddressTest extends FlatSpec with MustMatchers {
  val assetAddress = TestUtil.assetAddress
  "Addresses" must "be able to convert back and forth between a Bitcoin Address & an asset address" in {
    val convertedOnce = BitcoinAddress.convertToAssetAddress(TestUtil.bitcoinAddress)
    val actual : BitcoinAddress = AssetAddress.convertToBitcoinAddress(convertedOnce)
    actual must be (TestUtil.bitcoinAddress)
    val bitcoinAddress = AssetAddress.convertToBitcoinAddress(assetAddress)
    val actualAssetAddress = BitcoinAddress.convertToAssetAddress(bitcoinAddress)
    actualAssetAddress must be (assetAddress)
  }

  it must "allow type encapsulation for addresses" in {

    val bitcoinAddress : Address = TestUtil.bitcoinAddress
    val assetAddress : Address = TestUtil.assetAddress
    assetAddress must be (TestUtil.assetAddress)
    bitcoinAddress must be (TestUtil.bitcoinAddress)
  }
}
