package org.bitcoins.testkit.fixtures

import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.dlc.wallet.models.*
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.{BitcoinSTestAppConfig, PostgresTestDatabase}
import org.scalatest.*

trait DLCDAOFixture extends BitcoinSFixture with PostgresTestDatabase {

  private def daos()(implicit dlcAppConfig: DLCAppConfig): DLCDAOs = {
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
    val incomingDLCOfferDAO = IncomingDLCOfferDAO()
    val contactDAO = DLCContactDAO()
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
      dlcRemoteTxDAO = dlcRemoteTxDAO,
      incomingDLCOfferDAO = incomingDLCOfferDAO,
      contactDAO = contactDAO
    )
  }

  final override type FixtureParam = DLCDAOs

  def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig
      .getNeutrinoWithEmbeddedDbTestConfig(postgresOpt, Vector.empty)

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeDependentFixture[DLCDAOs](
      build = () => {
        val c = config.dlcConf
        c.start().map(_ => daos()(c))
      },
      destroy = { (daos: DLCDAOs) =>
        val config = daos.dlcConf
        for {
          _ <- config.dropAll()
          _ <- config.dropTable("flyway_schema_history")
          _ <- config.stop()
        } yield ()
      }
    )(test)
}
