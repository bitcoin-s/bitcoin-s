package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.hd._
import org.bitcoins.crypto.SchnorrNonce

case class RValueDb(
    nonce: SchnorrNonce,
    purpose: HDPurpose,
    accountCoin: HDCoinType,
    accountIndex: Int,
    chainType: HDChainType,
    keyIndex: Int) {

  lazy val hdAddress: HDAddress = {
    val hdCoin = HDCoin(purpose, accountCoin)
    val hdAccount = HDAccount(hdCoin, accountIndex)
    val hdChain = HDChain(chainType, hdAccount)
    HDAddress(hdChain, keyIndex)
  }
}

object RValueDbHelper {

  def apply(
      nonce: SchnorrNonce,
      account: HDAccount,
      chainType: HDChainType,
      keyIndex: Int): RValueDb = {
    RValueDb(nonce,
             account.purpose,
             account.coin.coinType,
             account.index,
             chainType,
             keyIndex)
  }
}
