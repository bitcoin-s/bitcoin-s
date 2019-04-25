package org.bitcoins.wallet.util

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{
  DoubleSha256Digest,
  ExtKeyPubVersion,
  ExtPublicKey
}
import org.bitcoins.core.crypto.bip44.{
  BIP44Account,
  BIP44ChainType,
  BIP44Coin,
  BIP44Path
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  RegTestNetChainParams
}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.wallet.models.AccountDb
import scodec.bits.HexStringSyntax

object WalletTestUtil {

  val chainParams: ChainParams = RegTestNetChainParams
  val networkParam: RegTest.type = RegTest

  val bip44Coin: BIP44Coin = BIP44Coin.fromChainParams(chainParams)
  lazy val sampleBip44Path = BIP44Path(bip44Coin,
                                       accountIndex = 0,
                                       BIP44ChainType.External,
                                       addressIndex = 0)

  val extKeyPubVersion: ExtKeyPubVersion =
    ExtKeyPubVersion.fromChainParams(chainParams)

  def freshXpub: ExtPublicKey =
    CryptoGenerators.extPublicKey.sample.getOrElse(freshXpub)

  val firstAccount = BIP44Account(bip44Coin, 0)
  def firstAccountDb = AccountDb(freshXpub, firstAccount)

  lazy val sampleTxid: DoubleSha256Digest = DoubleSha256Digest(
    hex"a910523c0b6752fbcb9c24303b4e068c505825d074a45d1c787122efb4649215")
  lazy val sampleVout: UInt32 = UInt32.zero
  lazy val sampleSPK: ScriptPubKey =
    ScriptPubKey.fromAsmBytes(hex"001401b2ac67587e4b603bb3ad709a8102c30113892d")
}
