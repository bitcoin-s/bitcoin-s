package org.bitcoins.dlc.wallet.models

import org.bitcoins.dlc.commons.oracle.{
  OracleAnnouncementDataDAO,
  OracleMetadataDAO,
  OracleSchnorrNonceDAO
}

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
    contactDAO: DLCContactDAO,
    oracleMetadataDAO: OracleMetadataDAO,
    oracleSchnorrNonceDAO: OracleSchnorrNonceDAO)
