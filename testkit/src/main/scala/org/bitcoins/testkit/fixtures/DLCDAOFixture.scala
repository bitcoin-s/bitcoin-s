package org.bitcoins.testkit.fixtures

import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.dlc.wallet.models._
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest._

import scala.concurrent.Future

case class DLCDAOs(
    announcementDAO: OracleAnnouncementDataDAO,
    nonceDAO: OracleNonceDAO,
    dlcAnnouncementDAO: DLCAnnouncementDAO,
    dlcDAO: DLCDAO,
    contractDataDAO: DLCContractDataDAO,
    dlcOfferDAO: DLCOfferDAO,
    dlcAcceptDAO: DLCAcceptDAO,
    dlcInputsDAO: DLCFundingInputDAO,
    dlcSigsDAO: DLCCETSignaturesDAO,
    dlcRefundSigDAO: DLCRefundSigsDAO,
    dlcRemoteTxDAO: DLCRemoteTxDAO)

trait DLCDAOFixture extends BitcoinSWalletTest {

  private lazy val daos: DLCDAOs = {
    val announcementDAO = OracleAnnouncementDataDAO()
    val nonceDAO = OracleNonceDAO()
    val dlcAnnouncementDAO = DLCAnnouncementDAO()
    val dlc = DLCDAO()
    val contractDataDAO = DLCContractDataDAO()
    val dlcOfferDAO = DLCOfferDAO()
    val dlcAcceptDAO = DLCAcceptDAO()
    val dlcInputsDAO = DLCFundingInputDAO()
    val dlcSigsDAO = DLCCETSignaturesDAO()
    val dlcRefundSigDAO = DLCRefundSigsDAO()
    val dlcRemoteTxDAO = DLCRemoteTxDAO()
    DLCDAOs(
      announcementDAO = announcementDAO,
      nonceDAO = nonceDAO,
      dlcAnnouncementDAO = dlcAnnouncementDAO,
      dlcDAO = dlc,
      contractDataDAO = contractDataDAO,
      dlcOfferDAO = dlcOfferDAO,
      dlcAcceptDAO = dlcAcceptDAO,
      dlcInputsDAO = dlcInputsDAO,
      dlcSigsDAO = dlcSigsDAO,
      dlcRefundSigDAO = dlcRefundSigDAO,
      dlcRemoteTxDAO = dlcRemoteTxDAO
    )
  }

  final override type FixtureParam = DLCDAOs

  implicit private val dlcConfig: DLCAppConfig = getFreshDLCAppConfig

  override def afterAll(): Unit = {
    super.afterAll()
  }

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeFixture(build = () => Future(dlcConfig.migrate()).map(_ => daos),
                destroy = () => dropAll())(test)

  def dropAll(): Future[Unit] = {
    val res = for {
      _ <- dlcConfig.dropTable("flyway_schema_history")
      _ <- dlcConfig.dropAll()
    } yield ()
    res.failed.foreach { ex =>
      ex.printStackTrace()
    }
    res
  }

}
