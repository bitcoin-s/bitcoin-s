package org.bitcoins.dlc.wallet.models
import org.bitcoins.dlc.wallet.DLCAppConfig

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
    dlcRemoteTxDAO: DLCRemoteTxDAO,
    incomingDLCOfferDAO: IncomingDLCOfferDAO,
    contactDAO: DLCContactDAO
)(implicit val dlcConf: DLCAppConfig) {

  val list = Vector(
    announcementDAO,
    nonceDAO,
    dlcDAO,
    dlcAnnouncementDAO,
    contractDataDAO,
    dlcOfferDAO,
    dlcAcceptDAO,
    dlcInputsDAO,
    dlcSigsDAO,
    dlcRefundSigDAO,
    dlcRemoteTxDAO,
    incomingDLCOfferDAO,
    contactDAO: DLCContactDAO
  )
}
