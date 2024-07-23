package org.bitcoins.wallet.models

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
