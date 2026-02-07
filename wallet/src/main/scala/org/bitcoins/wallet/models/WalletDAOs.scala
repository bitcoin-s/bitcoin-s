package org.bitcoins.wallet.models

import org.bitcoins.wallet.config.WalletAppConfig

case class WalletDAOs(
    accountDAO: AccountDAO,
    addressDAO: AddressDAO,
    addressTagDAO: AddressTagDAO,
    utxoDAO: SpendingInfoDAO,
    transactionDAO: TransactionDAO,
    incomingTxDAO: IncomingTransactionDAO,
    outgoingTxDAO: OutgoingTransactionDAO,
    scriptPubKeyDAO: ScriptPubKeyDAO,
    stateDescriptorDAO: WalletStateDescriptorDAO
) {

  val list = Vector(
    scriptPubKeyDAO,
    accountDAO,
    addressDAO,
    addressTagDAO,
    transactionDAO,
    incomingTxDAO,
    utxoDAO,
    outgoingTxDAO,
    stateDescriptorDAO
  )
}

object WalletDAOs {
  def fromWalletConfig(walletConfig: WalletAppConfig): WalletDAOs = {
    val addressDAO: AddressDAO =
      AddressDAO()(using walletConfig.ec, walletConfig)
    val accountDAO: AccountDAO =
      AccountDAO()(using walletConfig.ec, walletConfig)
    val spendingInfoDAO: SpendingInfoDAO =
      SpendingInfoDAO()(using walletConfig.ec, walletConfig)
    val transactionDAO: TransactionDAO =
      TransactionDAO()(using walletConfig.ec, walletConfig)
    val scriptPubKeyDAO: ScriptPubKeyDAO =
      ScriptPubKeyDAO()(using walletConfig.ec, walletConfig)

    val incomingTxDAO: IncomingTransactionDAO =
      IncomingTransactionDAO()(using walletConfig.ec, walletConfig)

    val outgoingTxDAO: OutgoingTransactionDAO =
      OutgoingTransactionDAO()(using walletConfig.ec, walletConfig)
    val addressTagDAO: AddressTagDAO =
      AddressTagDAO()(using walletConfig.ec, walletConfig)

    val stateDescriptorDAO: WalletStateDescriptorDAO =
      WalletStateDescriptorDAO()(using walletConfig.ec, walletConfig)
    WalletDAOs(accountDAO,
               addressDAO,
               addressTagDAO,
               spendingInfoDAO,
               transactionDAO,
               incomingTxDAO,
               outgoingTxDAO,
               scriptPubKeyDAO,
               stateDescriptorDAO)
  }
}
