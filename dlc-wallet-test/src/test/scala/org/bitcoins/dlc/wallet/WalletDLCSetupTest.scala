package org.bitcoins.dlc.wallet

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  DLCAccept,
  DLCOffer,
  DLCSign
}
import org.bitcoins.commons.jsonmodels.dlc.{CETSignatures, DLCMessage}
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future

class WalletDLCSetupTest extends BitcoinSDualWalletTest {
  type FixtureParam = (FundedDLCWallet, FundedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualFundedDLCWallets(test)
  }

  behavior of "DLCWallet"

  it must "correctly negotiate a dlc" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

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

        refundSigsA <-
          walletA.dlcRefundSigDAO.findByEventId(eventId).map(_.map(_.refundSig))
        refundSigsB <-
          walletB.dlcRefundSigDAO.findByEventId(eventId).map(_.map(_.refundSig))

        walletAChange <- walletA.addressDAO.read(offer.changeAddress)
        walletAFinal <- walletA.addressDAO.read(offer.pubKeys.payoutAddress)

        walletBChange <- walletB.addressDAO.read(accept.changeAddress)
        walletBFinal <- walletB.addressDAO.read(accept.pubKeys.payoutAddress)

      } yield {
        assert(dlcDb.eventId == sign.eventId)

        assert(refundSigsA.size == 2)
        assert(refundSigsA.forall(refundSigsB.contains))

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
      walletA: DLCWallet,
      walletB: DLCWallet,
      offerData: DLCOffer = DLCWalletUtil.sampleDLCOffer): Future[DLCSign] = {
    for {
      accept <- getDLCReadyToSign(walletA, walletB, offerData)
      sign <- walletA.signDLC(accept)
    } yield sign
  }

  def getDLCReadyToSign(
      walletA: DLCWallet,
      walletB: DLCWallet,
      offerData: DLCOffer = DLCWalletUtil.sampleDLCOffer): Future[DLCAccept] = {
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
    } yield accept
  }

  def testDLCSignVerification(
      walletA: DLCWallet,
      walletB: DLCWallet,
      makeDLCSignInvalid: DLCSign => DLCSign): Future[Assertion] = {
    val failedAddSigsF = for {
      sign <- getDLCReadyToAddSigs(walletA, walletB)
      invalidSign = makeDLCSignInvalid(sign)
      dlcDb <- walletB.addDLCSigs(invalidSign)
    } yield dlcDb

    recoverToSucceededIf[IllegalArgumentException](failedAddSigsF)
  }

  def testDLCAcceptVerification(
      walletA: DLCWallet,
      walletB: DLCWallet,
      makeDLCAcceptInvalid: DLCAccept => DLCAccept): Future[Assertion] = {
    val failedAddSigsF = for {
      accept <- getDLCReadyToSign(walletA, walletB)
      invalidSign = makeDLCAcceptInvalid(accept)
      dlcDb <- walletA.signDLC(invalidSign)
    } yield dlcDb

    recoverToSucceededIf[IllegalArgumentException](failedAddSigsF)
  }

  it must "fail to add dlc funding sigs that are invalid" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      testDLCSignVerification(
        walletA,
        walletB,
        (sign: DLCSign) =>
          sign.copy(fundingSigs = DLCWalletUtil.dummyFundingSignatures))
  }

  it must "fail to add dlc cet sigs that are invalid" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      testDLCSignVerification(
        walletA,
        walletB,
        (sign: DLCSign) =>
          sign.copy(
            cetSigs = CETSignatures(DLCWalletUtil.dummyOutcomeSigs,
                                    sign.cetSigs.refundSig)))
  }

  it must "fail to add an invalid dlc refund sig" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      testDLCSignVerification(
        walletA,
        walletB,
        (sign: DLCSign) =>
          sign.copy(
            cetSigs = CETSignatures(sign.cetSigs.outcomeSigs,
                                    DLCWalletUtil.dummyPartialSig)))
  }

  it must "fail to sign dlc with cet sigs that are invalid" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      testDLCAcceptVerification(
        walletA,
        walletB,
        (accept: DLCAccept) =>
          accept.copy(
            cetSigs = CETSignatures(DLCWalletUtil.dummyOutcomeSigs,
                                    accept.cetSigs.refundSig)))
  }

  it must "fail to sign dlc with an invalid refund sig" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      testDLCAcceptVerification(
        walletA,
        walletB,
        (accept: DLCAccept) =>
          accept.copy(
            cetSigs = CETSignatures(accept.cetSigs.outcomeSigs,
                                    DLCWalletUtil.dummyPartialSig)))
  }
}
