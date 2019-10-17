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
}
