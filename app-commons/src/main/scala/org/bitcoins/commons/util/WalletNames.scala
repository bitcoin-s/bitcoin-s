package org.bitcoins.commons.util

object WalletNames {

  val WalletNameMaxLen: Int = 56 // == (63 - "wallet_".length)

  def validateWalletName(walletName: String): Boolean = {
    walletName.length <= WalletNameMaxLen &&
    walletName.forall { char =>
      char.isLetterOrDigit || char == '_' || char == '-'
    }
  }
}
