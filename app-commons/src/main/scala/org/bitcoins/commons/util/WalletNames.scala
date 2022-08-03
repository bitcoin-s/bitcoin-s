package org.bitcoins.commons.util

object WalletNames {

  val walletNameMaxLen: Int = 56 // == (63 - "wallet_".length)

  def validateWalletName(walletName: String): Boolean = {
    walletName.length <= walletNameMaxLen &&
    walletName.forall { char =>
      char.isLetterOrDigit || char == '_'
    }
  }
}
