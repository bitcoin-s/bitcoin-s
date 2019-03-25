package org.bitcoins.wallet.util

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{ExtKeyPubVersion, ExtPublicKey}
import org.bitcoins.core.crypto.bip44.{BIP44Account, BIP44Coin}
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  RegTestNetChainParams
}
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.wallet.models.AccountDb

object WalletTestUtil {

  val chainParams: ChainParams = RegTestNetChainParams
  val networkParam: RegTest.type = RegTest
  val bip44Coin = BIP44Coin.fromChainParams(chainParams)

  val extKeyPubVersion: ExtKeyPubVersion =
    ExtKeyPubVersion.fromChainParams(chainParams)

  def freshXpub: ExtPublicKey =
    CryptoGenerators.extPublicKey.sample.getOrElse(freshXpub)

  val firstAccount = BIP44Account(bip44Coin, 0)
  def firstAccountDb = AccountDb(freshXpub, firstAccount)
}
