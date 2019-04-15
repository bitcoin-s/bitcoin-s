package org.bitcoins.wallet.util

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{
  DoubleSha256Digest,
  ExtKeyPubVersion,
  ExtPublicKey,
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
import org.bitcoins.core.hd.HDCoin
import org.bitcoins.core.hd.HDCoinType
import org.bitcoins.core.hd.HDPath
import org.bitcoins.core.hd.SegWitHDPath
import org.bitcoins.core.hd.HDChain
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.hd.HDPurposes

object WalletTestUtil {

  val chainParams: ChainParams = RegTestNetChainParams
  val networkParam: RegTest.type = RegTest

  val hdCoinType: HDCoinType = ???

  lazy val sampleSegwitPath =
    SegWitHDPath(hdCoinType,
                 accountIndex = 0,
                 HDChainType.External,
                 addressIndex = 0)

  val extKeyPubVersion: ExtKeyPubVersion =
    ExtKeyPubVersion.fromChainParams(chainParams)

  def freshXpub: ExtPublicKey =
    CryptoGenerators.extPublicKey.sample.getOrElse(freshXpub)

  val firstAccount = HDAccount(HDCoin(HDPurposes.SegWit, hdCoinType), 0)
  def firstAccountDb = AccountDb(freshXpub, firstAccount)

  lazy val sampleTxid: DoubleSha256Digest = DoubleSha256Digest(
    hex"a910523c0b6752fbcb9c24303b4e068c505825d074a45d1c787122efb4649215")
  lazy val sampleVout: UInt32 = UInt32.zero
  lazy val sampleSPK: ScriptPubKey =
    ScriptPubKey.fromAsmBytes(hex"001401b2ac67587e4b603bb3ad709a8102c30113892d")
}
