package org.bitcoins.dlc.wallet.models

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
    dlcRemoteTxDAO: DLCRemoteTxDAO)
