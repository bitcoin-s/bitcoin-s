package org.bitcoins.core.protocol.transaction

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class TaprootWalletTestVectors extends BitcoinSUnitTest {
  behavior of "TaprootWalletTestVectors"

  //from: https://github.com/bitcoin/bips/blob/master/bip-0341/wallet-test-vectors.json
  lazy val url = getClass.getResource("/wallet-test-vectors.json")

  lazy val lines = {
    scala.io.Source.fromURL(url).getLines().mkString
  }

  lazy val testCase: TaprootWalletTestCases = {
    upickle.default.read[TaprootWalletTestCases](lines)(
      TaprootWalletTestCase.walletTestVectorReader)
  }

  lazy val tests = testCase.tests

  it must "build correct taproot spks" in {
    tests.foreach(t => println(t))
    succeed

  }
}
