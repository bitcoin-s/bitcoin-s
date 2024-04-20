package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.config.MainNet
import org.bitcoins.core.protocol.Bech32mAddress
import org.bitcoins.core.protocol.script.TaprootScriptPubKey
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalatest.Assertion

class TaprootWalletTestVectors extends BitcoinSUnitTest {
  behavior of "TaprootWalletTestVectors"

  // from: https://github.com/bitcoin/bips/blob/master/bip-0341/wallet-test-vectors.json
  lazy val url = getClass.getResource("/wallet-test-vectors.json")

  lazy val lines = {
    scala.io.Source.fromURL(url).getLines().mkString
  }

  lazy val testCase: TaprootWalletTestCases = {
    upickle.default.read[TaprootWalletTestCases](lines)(
      TaprootWalletTestCase.walletTestVectorReader
    )
  }

  lazy val tests: Vector[TaprootWalletTestCase] = testCase.tests

  it must "build correct taproot spks" in {
    tests.foreach { test =>
      val `given` = test.`given`
      val intermediary = test.intermediary
      if (`given`.treeOpt.isEmpty) {
        checkOutput(test)
      } else {
        val leafHashes = `given`.leafHashes
        assert(leafHashes == intermediary.leafHashes.get)
        assert(
          `given`.merkleRootOpt == intermediary.merkleRootOpt,
          s"test=${test.expected.bip350Address}"
        )
        checkOutput(test)
      }
    }
  }

  private def checkOutput(test: TaprootWalletTestCase): Assertion = {
    val `given` = test.`given`
    val intermediary = test.intermediary
    val expected = test.expected
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
  }
}
