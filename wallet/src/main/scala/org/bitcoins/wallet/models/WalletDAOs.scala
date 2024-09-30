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
    val addressDAO: AddressDAO = AddressDAO()(walletConfig.ec, walletConfig)
    val accountDAO: AccountDAO = AccountDAO()(walletConfig.ec, walletConfig)
    val spendingInfoDAO: SpendingInfoDAO =
      SpendingInfoDAO()(walletConfig.ec, walletConfig)
    val transactionDAO: TransactionDAO =
      TransactionDAO()(walletConfig.ec, walletConfig)
    val scriptPubKeyDAO: ScriptPubKeyDAO =
      ScriptPubKeyDAO()(walletConfig.ec, walletConfig)

    val incomingTxDAO: IncomingTransactionDAO =
      IncomingTransactionDAO()(walletConfig.ec, walletConfig)

    val outgoingTxDAO: OutgoingTransactionDAO =
      OutgoingTransactionDAO()(walletConfig.ec, walletConfig)
    val addressTagDAO: AddressTagDAO =
      AddressTagDAO()(walletConfig.ec, walletConfig)

    val stateDescriptorDAO: WalletStateDescriptorDAO =
      WalletStateDescriptorDAO()(walletConfig.ec, walletConfig)
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
