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

    assert(fromMain == MainNetPriv)
    assert(fromTest == TestNet3Priv)
    assert(fromReg == TestNet3Priv)
  }

  "ExtKeyPubVersion" must "derive correct key versions from chain params" in {
    val fromMain = ExtKeyPubVersion.fromChainParams(MainNetChainParams)
    val fromTest = ExtKeyPubVersion.fromChainParams(TestNetChainParams)
    val fromReg = ExtKeyPubVersion.fromChainParams(RegTestNetChainParams)

    assert(fromMain == MainNetPub)
    assert(fromTest == TestNet3Pub)
    assert(fromReg == TestNet3Pub)

  }

}
