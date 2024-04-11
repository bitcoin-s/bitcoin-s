package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.config.MainNet
import org.bitcoins.core.protocol.Bech32mAddress
import org.bitcoins.core.protocol.script.TaprootScriptPubKey
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
    tests.foreach { test =>
      val `given` = test.`given`
      val intermediary = test.intermediary
      val expected = test.expected
      if (`given`.scriptTrees.isEmpty) {
        //val expectedAddr = test.expected.bip350Address
        //val expectedSPK = test.expected.scriptPubKey
        val internal = `given`.internalPubkey
        val tweakHash =
          internal.computeTapTweakHash(intermediary.merkleRootOpt)
        assert(tweakHash == test.intermediary.tweak)
        val (_, tweakedKey) =
          internal.createTapTweak(intermediary.merkleRootOpt)
        assert(tweakedKey == intermediary.tweakedPubkey)
        val spk = TaprootScriptPubKey.fromPubKey(tweakedKey)
        val addr = Bech32mAddress.fromScriptPubKey(spk, MainNet)
        assert(spk == expected.scriptPubKey)
        assert(addr == expected.bip350Address)
      } else {
        fail(s"test=$test")
      }
    }

  }
}
