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
    val dlcDAO = DLCDAO()(dlcConfig.ec, dlcConfig)
    val contractDAO = DLCContractDataDAO()(dlcConfig.ec, dlcConfig)
    val dlcAnnouncementDAO = DLCAnnouncementDAO()(dlcConfig.ec, dlcConfig)
    val dlcInputsDAO = DLCFundingInputDAO()(dlcConfig.ec, dlcConfig)
    val dlcOfferDAO = DLCOfferDAO()(dlcConfig.ec, dlcConfig)
    val dlcAcceptDAO = DLCAcceptDAO()(dlcConfig.ec, dlcConfig)
    val dlcSigsDAO = DLCCETSignaturesDAO()(dlcConfig.ec, dlcConfig)
    val dlcRefundSigDAO = DLCRefundSigsDAO()(dlcConfig.ec, dlcConfig)
    val oracleNonceDAO = OracleNonceDAO()(dlcConfig.ec, dlcConfig)
    val oracleAnnouncementDAO =
      OracleAnnouncementDataDAO()(dlcConfig.ec, dlcConfig)
    val dlcRemoteTxDAO = DLCRemoteTxDAO()(dlcConfig.ec, dlcConfig)
    val incomingDLCOfferDAO = IncomingDLCOfferDAO()(dlcConfig.ec, dlcConfig)
    val contactDAO = DLCContactDAO()(dlcConfig.ec, dlcConfig)
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
