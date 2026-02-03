package org.bitcoins.dlc.wallet.models

import org.bitcoins.dlc.wallet.DLCAppConfig

case class DLCWalletDAOs(
    dlcDAO: DLCDAO,
    contractDataDAO: DLCContractDataDAO,
    dlcAnnouncementDAO: DLCAnnouncementDAO,
    dlcInputsDAO: DLCFundingInputDAO,
    dlcOfferDAO: DLCOfferDAO,
    dlcAcceptDAO: DLCAcceptDAO,
    dlcSigsDAO: DLCCETSignaturesDAO,
    dlcRefundSigDAO: DLCRefundSigsDAO,
    oracleNonceDAO: OracleNonceDAO,
    oracleAnnouncementDAO: OracleAnnouncementDataDAO,
    dlcRemoteTxDAO: DLCRemoteTxDAO,
    incomingDLCOfferDAO: IncomingDLCOfferDAO,
    contactDAO: DLCContactDAO
)

object DLCWalletDAOs {
  def fromDLCAppConfig(dlcConfig: DLCAppConfig): DLCWalletDAOs = {
    val dlcDAO = DLCDAO()(using dlcConfig.ec, dlcConfig)
    val contractDAO = DLCContractDataDAO()(using dlcConfig.ec, dlcConfig)
    val dlcAnnouncementDAO = DLCAnnouncementDAO()(using dlcConfig.ec, dlcConfig)
    val dlcInputsDAO = DLCFundingInputDAO()(using dlcConfig.ec, dlcConfig)
    val dlcOfferDAO = DLCOfferDAO()(using dlcConfig.ec, dlcConfig)
    val dlcAcceptDAO = DLCAcceptDAO()(using dlcConfig.ec, dlcConfig)
    val dlcSigsDAO = DLCCETSignaturesDAO()(using dlcConfig.ec, dlcConfig)
    val dlcRefundSigDAO = DLCRefundSigsDAO()(using dlcConfig.ec, dlcConfig)
    val oracleNonceDAO = OracleNonceDAO()(using dlcConfig.ec, dlcConfig)
    val oracleAnnouncementDAO =
      OracleAnnouncementDataDAO()(using dlcConfig.ec, dlcConfig)
    val dlcRemoteTxDAO = DLCRemoteTxDAO()(using dlcConfig.ec, dlcConfig)
    val incomingDLCOfferDAO = IncomingDLCOfferDAO()(using dlcConfig.ec, dlcConfig)
    val contactDAO = DLCContactDAO()(using dlcConfig.ec, dlcConfig)
    DLCWalletDAOs(
      dlcDAO = dlcDAO,
      contractDataDAO = contractDAO,
      dlcAnnouncementDAO = dlcAnnouncementDAO,
      dlcInputsDAO = dlcInputsDAO,
      dlcOfferDAO = dlcOfferDAO,
      dlcAcceptDAO = dlcAcceptDAO,
      dlcSigsDAO = dlcSigsDAO,
      dlcRefundSigDAO = dlcRefundSigDAO,
      oracleNonceDAO = oracleNonceDAO,
      oracleAnnouncementDAO = oracleAnnouncementDAO,
      dlcRemoteTxDAO = dlcRemoteTxDAO,
      incomingDLCOfferDAO = incomingDLCOfferDAO,
      contactDAO = contactDAO
    )
  }
}
