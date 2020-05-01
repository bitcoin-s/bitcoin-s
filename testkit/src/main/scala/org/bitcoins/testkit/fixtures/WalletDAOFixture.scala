package org.bitcoins.testkit.fixtures

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models._
import org.scalatest._
import org.scalatest.flatspec.FixtureAsyncFlatSpec

case class WalletDAOs(
    accountDAO: AccountDAO,
    addressDAO: AddressDAO,
    utxoDAO: SpendingInfoDAO,
    transactionDAO: TransactionDAO,
    incomingTxDAO: IncomingTransactionDAO,
    outgoingTxDAO: OutgoingTransactionDAO,
    dlcDAO: DLCDAO,
    dlcOfferDAO: DLCOfferDAO,
    dlcAcceptDAO: DLCAcceptDAO,
    dlcInputsDAO: DLCFundingInputDAO,
    dlcSigsDAO: DLCCETSignatureDAO)

trait WalletDAOFixture extends FixtureAsyncFlatSpec with BitcoinSWalletTest {

  private lazy val daos: WalletDAOs = {
    val account = AccountDAO()
    val address = AddressDAO()
    val utxo = SpendingInfoDAO()
    val tx = TransactionDAO()
    val incomingTx = IncomingTransactionDAO()
    val outgoingTx = OutgoingTransactionDAO()
    val dlc = DLCDAO()
    val dlcOfferDAO = DLCOfferDAO()
    val dlcAcceptDAO = DLCAcceptDAO()
    val dlcInputsDAO = DLCFundingInputDAO()
    val dlcSigsDAO = DLCCETSignatureDAO()
    WalletDAOs(
      accountDAO = account,
      addressDAO = address,
      utxoDAO = utxo,
      transactionDAO = tx,
      incomingTxDAO = incomingTx,
      outgoingTxDAO = outgoingTx,
      dlcDAO = dlc,
      dlcOfferDAO = dlcOfferDAO,
      dlcAcceptDAO = dlcAcceptDAO,
      dlcInputsDAO = dlcInputsDAO,
      dlcSigsDAO = dlcSigsDAO
    )
  }

  final override type FixtureParam = WalletDAOs

  implicit private val walletConfig: WalletAppConfig = config

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeFixture(build = () => walletConfig.createAll().map(_ => daos),
                destroy = () => walletConfig.dropAll())(test)
}
