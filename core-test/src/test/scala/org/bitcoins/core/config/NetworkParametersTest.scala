package org.bitcoins.core.config

import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 6/10/16.
  */
class NetworkParametersTest extends BitcoinSUnitTest {

  //test case answers are from this link
  //https://en.bitcoin.it/wiki/Protocol_documentation#Common_structures
  "NetworkParameters" must "create the correct magic network bytes for mainnet" in {
    BitcoinSUtil.encodeHex(MainNet.magicBytes) must be("f9beb4d9")
  }

  it must "create the correct magic network bytes for testnet" in {
    BitcoinSUtil.encodeHex(TestNet3.magicBytes) must be("0B110907".toLowerCase)
  }

  it must "create the correct magic network bytes for regtest" in {
    BitcoinSUtil.encodeHex(RegTest.magicBytes) must be("fabfb5da")
  }

  it must "create the correct magic network bytes for signet" in {
    BitcoinSUtil.encodeHex(SigNet.magicBytes) must be("f0c7706a")
  }

  it must "get the correct Network from string" in {
    assert(Networks.fromString("mainnet").contains(MainNet))
    assert(Networks.fromString("testnet").contains(TestNet3))
    assert(Networks.fromString("regtest").contains(RegTest))
    assert(Networks.fromString("signet").contains(SigNet))
    assert(Networks.fromString("").isEmpty)
    assert(Networks.fromString("craig wright is a fraud").isEmpty)
  }
}
