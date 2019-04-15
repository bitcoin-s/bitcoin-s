package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.blockchain.{
  MainNetChainParams,
  RegTestNetChainParams,
  TestNetChainParams
}
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.core.crypto.ExtKeyVersion._

class ExtKeyVersionTest extends BitcoinSUnitTest {

  "ExtKeyPrivVersion" must "derive correct key versions from chain params" in {
    val fromMain = ExtKeyPrivVersion.fromChainParams(MainNetChainParams)
    val fromTest = ExtKeyPrivVersion.fromChainParams(TestNetChainParams)
    val fromReg = ExtKeyPrivVersion.fromChainParams(RegTestNetChainParams)

    assert(fromMain == LegacyMainNetPriv)
    assert(fromTest == LegacyTestNet3Priv)
    assert(fromReg == LegacyTestNet3Priv)
  }

  "ExtKeyPubVersion" must "derive correct key versions from chain params" in {
    val fromMain = ExtKeyPubVersion.fromChainParams(MainNetChainParams)
    val fromTest = ExtKeyPubVersion.fromChainParams(TestNetChainParams)
    val fromReg = ExtKeyPubVersion.fromChainParams(RegTestNetChainParams)

    assert(fromMain == LegacyMainNetPub)
    assert(fromTest == LegacyTestNet3Pub)
    assert(fromReg == LegacyTestNet3Pub)

  }

}
