package org.bitcoins.wallet.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, DLCWalletUtil}
import org.scalatest.FutureOutcome

class WalletDLCSetupTest extends BitcoinSWalletTest {
  type FixtureParam = FundedWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedSegwitWallet(test)
  }

  behavior of "DLCWallet"

  it must "create a dlc offer" in { funded: FundedWallet =>
    val fundedWallet = funded.wallet

    val offerData = DLCWalletUtil.sampleDLCOffer

    for {
      offer <- fundedWallet.registerDLCOffer(offerData)
    } yield {
      assert(offer.oracleInfo == offerData.oracleInfo)
      assert(offer.contractInfo == offerData.contractInfo)
      assert(offer.totalCollateral == offerData.totalCollateral)
      assert(offer.feeRate == offerData.feeRate)
      assert(offer.timeouts == offerData.timeouts)
      assert(offer.fundingInputs.nonEmpty)
      assert(offer.changeAddress.value.nonEmpty)
    }
  }

  it must "accept a dlc offer" in { funded: FundedWallet =>
    val fundedWallet = funded.wallet

    val offer = DLCWalletUtil.sampleDLCOffer
    val eventId = DLCMessage.calcEventId(offer.oracleInfo,
                                         offer.contractInfo,
                                         offer.timeouts)

    for {
      accept <- fundedWallet.acceptDLCOffer(offer)
    } yield {
      assert(accept.fundingInputs.nonEmpty)
      assert(accept.eventId == eventId)
      assert(
        accept.totalCollateral == offer.contractInfo.values.max - offer.totalCollateral)
      assert(accept.changeAddress.value.nonEmpty)
    }
  }

  it must "sign a dlc" in { funded: FundedWallet =>
    val fundedWallet = funded.wallet

    val offerData = DLCWalletUtil.sampleDLCOffer
    val accept = DLCWalletUtil.sampleDLCAccept

    for {
      _ <- fundedWallet.createDLCOffer(
        offerData.oracleInfo,
        offerData.contractInfo,
        offerData.totalCollateral,
        Some(offerData.feeRate),
        offerData.timeouts.contractMaturity.toUInt32,
        offerData.timeouts.contractTimeout.toUInt32
      ) // must initialize the dlc in the database

      sign <- fundedWallet.signDLC(accept)
    } yield {
      assert(sign.eventId == accept.eventId)
      assert(sign.fundingSigs.keys.size == offerData.fundingInputs.size)
    }
  }

  it must "add dlc sigs" in { funded: FundedWallet =>
    val fundedWallet = funded.wallet

    val offer = DLCWalletUtil.sampleDLCOffer
    val sigs = DLCWalletUtil.sampleDLCSign

    for {
      _ <- fundedWallet.acceptDLCOffer(offer) // must initialize the dlc in the database

      dlcDb <- fundedWallet.addDLCSigs(sigs)
      outcomeSigs <- fundedWallet.dlcSigsDAO.findByEventId(sigs.eventId)
    } yield {
      assert(dlcDb.eventId == sigs.eventId)
      assert(dlcDb.refundSigOpt.isDefined)
      assert(dlcDb.refundSigOpt.get === sigs.cetSigs.refundSig)
      assert(sigs.cetSigs.outcomeSigs.forall(sig =>
        outcomeSigs.exists(_.toTuple == sig)))
    }
  }
}
