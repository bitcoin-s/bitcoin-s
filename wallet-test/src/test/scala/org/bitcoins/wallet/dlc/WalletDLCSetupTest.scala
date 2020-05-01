package org.bitcoins.wallet.dlc

import org.bitcoins.commons.jsonmodels.dlc.{CETSignatures, DLCMessage}
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{DLCOffer, DLCSign}
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.bitcoins.wallet.Wallet
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future

class WalletDLCSetupTest extends BitcoinSDualWalletTest {
  type FixtureParam = (FundedWallet, FundedWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualFundedSegwitWallets(test)
  }

  behavior of "DLCWallet"

  it must "correctly negotiate a dlc" in {
    fundedWallets: (FundedWallet, FundedWallet) =>
      val walletA = fundedWallets._1.wallet
      val walletB = fundedWallets._2.wallet

      val offerData = DLCWalletUtil.sampleDLCOffer
      val eventId = DLCMessage.calcEventId(offerData.oracleInfo,
                                           offerData.contractInfo,
                                           offerData.timeouts)

      for {
        offer <- walletA.createDLCOffer(
          offerData.oracleInfo,
          offerData.contractInfo,
          offerData.totalCollateral,
          Some(offerData.feeRate),
          offerData.timeouts.contractMaturity.toUInt32,
          offerData.timeouts.contractTimeout.toUInt32
        )
        _ = {
          assert(offer.oracleInfo == offerData.oracleInfo)
          assert(offer.contractInfo == offerData.contractInfo)
          assert(offer.totalCollateral == offerData.totalCollateral)
          assert(offer.feeRate == offerData.feeRate)
          assert(offer.timeouts == offerData.timeouts)
          assert(offer.fundingInputs.nonEmpty)
          assert(offer.changeAddress.value.nonEmpty)
        }

        accept <- walletB.acceptDLCOffer(offer)
        _ = {
          assert(accept.fundingInputs.nonEmpty)
          assert(accept.eventId == eventId)
          assert(
            accept.totalCollateral == offer.contractInfo.values.max - offer.totalCollateral)
          assert(accept.changeAddress.value.nonEmpty)
        }

        sign <- walletA.signDLC(accept)
        _ = {
          assert(sign.eventId == accept.eventId)
          assert(sign.fundingSigs.keys.size == offerData.fundingInputs.size)
        }

        dlcDb <- walletB.addDLCSigs(sign)
        outcomeSigs <- walletB.dlcSigsDAO.findByEventId(sign.eventId)

        walletAChange <- walletA.addressDAO.read(offer.changeAddress)
        walletAFinal <- walletA.addressDAO.read(offer.pubKeys.finalAddress)

        walletBChange <- walletB.addressDAO.read(accept.changeAddress)
        walletBFinal <- walletB.addressDAO.read(accept.pubKeys.finalAddress)

      } yield {
        assert(dlcDb.eventId == sign.eventId)
        assert(dlcDb.refundSigOpt.isDefined)
        assert(dlcDb.refundSigOpt.get === sign.cetSigs.refundSig)
        assert(sign.cetSigs.outcomeSigs.forall(sig =>
          outcomeSigs.exists(_.toTuple == sig)))

        // Test that the Addresses are in the wallet's database
        assert(walletAChange.isDefined)
        assert(walletAFinal.isDefined)
        assert(walletBChange.isDefined)
        assert(walletBFinal.isDefined)
      }
  }

  def getDLCReadyToAddSigs(
      walletA: Wallet,
      walletB: Wallet,
      offerData: DLCOffer = DLCWalletUtil.sampleDLCOffer): Future[DLCSign] = {
    for {
      offer <- walletA.createDLCOffer(
        offerData.oracleInfo,
        offerData.contractInfo,
        offerData.totalCollateral,
        Some(offerData.feeRate),
        offerData.timeouts.contractMaturity.toUInt32,
        offerData.timeouts.contractTimeout.toUInt32
      )

      accept <- walletB.acceptDLCOffer(offer)
      sign <- walletA.signDLC(accept)
    } yield sign
  }

  def testDLCSignVerification(
      walletA: Wallet,
      walletB: Wallet,
      makeDLCSignInvalid: DLCSign => DLCSign): Future[Assertion] = {
    val failedAddSigsF = for {
      sign <- getDLCReadyToAddSigs(walletA, walletB)
      invalidSign = makeDLCSignInvalid(sign)
      dlcDb <- walletB.addDLCSigs(invalidSign)
    } yield dlcDb

    recoverToSucceededIf[IllegalArgumentException](failedAddSigsF)
  }

  it must "fail to add dlc funding sigs that are invalid" in {
    fundedWallets: (FundedWallet, FundedWallet) =>
      val walletA = fundedWallets._1.wallet
      val walletB = fundedWallets._2.wallet

      testDLCSignVerification(
        walletA,
        walletB,
        (sign: DLCSign) =>
          sign.copy(fundingSigs = DLCWalletUtil.dummyFundingSignatures))
  }

  it must "fail to add dlc cet sigs that are invalid" in {
    fundedWallets: (FundedWallet, FundedWallet) =>
      val walletA = fundedWallets._1.wallet
      val walletB = fundedWallets._2.wallet

      testDLCSignVerification(
        walletA,
        walletB,
        (sign: DLCSign) =>
          sign.copy(
            cetSigs = CETSignatures(DLCWalletUtil.dummyOutcomeSigs,
                                    sign.cetSigs.refundSig)))
  }

  it must "fail to add an invalid dlc refund sig" in {
    fundedWallets: (FundedWallet, FundedWallet) =>
      val walletA = fundedWallets._1.wallet
      val walletB = fundedWallets._2.wallet

      testDLCSignVerification(
        walletA,
        walletB,
        (sign: DLCSign) =>
          sign.copy(
            cetSigs = CETSignatures(sign.cetSigs.outcomeSigs,
                                    DLCWalletUtil.dummyPartialSig)))
  }
}
