package org.bitcoins.dlc.wallet.internal

import org.bitcoins.core.protocol.dlc.compute.DLCUtil
import org.bitcoins.core.protocol.dlc.models.DLCMessage.DLCOffer
import org.bitcoins.core.protocol.dlc.models.DLCState
import org.bitcoins.dlc.wallet.models.{AcceptDbState, SignDbState}
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.scalatest.FutureOutcome

class DLCDataManagementTest extends BitcoinSDualWalletTest {
  type FixtureParam = (FundedDLCWallet, FundedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualFundedDLCWallets(test)
  }

  behavior of "DLCDataManagement"

  it must "retrieve a acceptdb state from getDLCFundingData" in {
    fundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = fundedDLCWallets._1.wallet
      val walletB = fundedDLCWallets._2.wallet

      val offerData: DLCOffer =
        DLCWalletUtil.sampleDLCOffer

      for {
        offer1 <- walletA.createDLCOffer(
          offerData.contractInfo,
          offerData.collateral,
          Some(offerData.feeRate),
          offerData.timeouts.contractMaturity.toUInt32,
          offerData.timeouts.contractTimeout.toUInt32,
          None,
          None,
          None
        )
        accept <- walletB.acceptDLCOffer(offer1, None, None, None)
        contractId = DLCUtil.calcContractId(offer1, accept)
        acceptDbStateOpt <- walletB.dlcDataManagement.getDLCFundingData(
          contractId,
          walletA.transactionDAO)
      } yield {
        assert(acceptDbStateOpt.isDefined)
        assert(acceptDbStateOpt.get.isInstanceOf[AcceptDbState])
        assert(acceptDbStateOpt.get.state == DLCState.Accepted)
      }
  }
  it must "retrieve a signdb state from getDLCFundingData" in {
    fundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = fundedDLCWallets._1.wallet
      val walletB = fundedDLCWallets._2.wallet

      val offerData: DLCOffer =
        DLCWalletUtil.sampleDLCOffer

      for {
        offer1 <- walletA.createDLCOffer(
          offerData.contractInfo,
          offerData.collateral,
          Some(offerData.feeRate),
          offerData.timeouts.contractMaturity.toUInt32,
          offerData.timeouts.contractTimeout.toUInt32,
          None,
          None,
          None
        )
        accept <- walletB.acceptDLCOffer(offer1, None, None, None)

        sign <- walletA.signDLC(accept)
        signDbStateOpt <- walletA.dlcDataManagement.getDLCFundingData(
          sign.contractId,
          walletA.transactionDAO)
      } yield {
        assert(signDbStateOpt.isDefined)
        assert(signDbStateOpt.get.isInstanceOf[SignDbState])
        assert(signDbStateOpt.get.state == DLCState.Signed)
      }
  }
}
