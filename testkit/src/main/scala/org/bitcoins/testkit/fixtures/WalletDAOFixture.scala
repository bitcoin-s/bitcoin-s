package org.bitcoins.testkit.fixtures

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest._
import org.scalatest.flatspec.FixtureAsyncFlatSpec
import org.bitcoins.wallet.models._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.db.WalletDbManagement

case class WalletDAOs(
    accountDAO: AccountDAO,
    addressDAO: AddressDAO,
    utxoDAO: SpendingInfoDAO,
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
    val dlc = DLCDAO()
    val dlcOfferDAO = DLCOfferDAO()
    val dlcAcceptDAO = DLCAcceptDAO()
    val dlcInputsDAO = DLCFundingInputDAO()
    val dlcSigsDAO = DLCCETSignatureDAO()
    WalletDAOs(
      accountDAO = account,
      addressDAO = address,
      utxoDAO = utxo,
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
    makeFixture(build = () => WalletDbManagement.createAll().map(_ => daos),
                destroy = () => WalletDbManagement.dropAll())(test)
}
