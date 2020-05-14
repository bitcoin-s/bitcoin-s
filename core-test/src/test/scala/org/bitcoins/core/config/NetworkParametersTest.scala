package org.bitcoins.core.config

import org.bitcoins.crypto.BytesUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 6/10/16.
  */
class NetworkParametersTest extends BitcoinSUnitTest {

  //test case answers are from this link
  //https://en.bitcoin.it/wiki/Protocol_documentation#Common_structures
  "NetworkParameters" must "create the correct magic network bytes for mainnet" in {
    BytesUtil.encodeHex(MainNet.magicBytes) must be("f9beb4d9")
  }

  it must "create the correct magic network bytes for testnet" in {
    BytesUtil.encodeHex(TestNet3.magicBytes) must be("0B110907".toLowerCase)
  }

  it must "create the correct magic network bytes for regtest" in {
    BytesUtil.encodeHex(RegTest.magicBytes) must be("fabfb5da")
  }

  it must "get the correct Network from string" in {
    assert(Networks.fromString("mainnet").contains(MainNet))
    assert(Networks.fromString("testnet").contains(TestNet3))
    assert(Networks.fromString("regtest").contains(RegTest))
    assert(Networks.fromString("").isEmpty)
    assert(Networks.fromString("craig wright is a fraud").isEmpty)
  }
}
