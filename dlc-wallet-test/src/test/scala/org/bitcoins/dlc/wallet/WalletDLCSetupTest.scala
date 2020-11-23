package org.bitcoins.dlc.wallet

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.testkit.wallet.DLCWalletUtil._
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

  def testNegotiate(
      fundedDLCWallets: (FundedDLCWallet, FundedDLCWallet),
      offerData: DLCOffer): Future[Assertion] = {
    val walletA = fundedDLCWallets._1.wallet
    val walletB = fundedDLCWallets._2.wallet

    for {
      offer <- walletA.createDLCOffer(
        offerData.oracleInfo,
        offerData.contractInfo,
        offerData.totalCollateral,
        Some(offerData.feeRate),
        offerData.timeouts.contractMaturity.toUInt32,
        offerData.timeouts.contractTimeout.toUInt32
      )
      paramHash = offer.paramHash
      dlcA1Opt <- walletA.dlcDAO.read(paramHash)
      find1 <- walletA.findDLC(paramHash)
      _ = {
        assert(dlcA1Opt.isDefined)
        assert(find1.isDefined)
        assert(dlcA1Opt.get.state == DLCState.Offered)
        assert(offer.oracleInfo == offerData.oracleInfo)
        assert(offer.contractInfo == offerData.contractInfo)
        assert(offer.totalCollateral == offerData.totalCollateral)
        assert(offer.feeRate == offerData.feeRate)
        assert(offer.timeouts == offerData.timeouts)
        assert(offer.fundingInputs.nonEmpty)
        assert(offer.changeAddress.value.nonEmpty)
      }

      accept <- walletB.acceptDLCOffer(offer)
      dlcB1Opt <- walletB.dlcDAO.read(paramHash)
      _ = {
        assert(dlcB1Opt.isDefined)
        assert(dlcB1Opt.get.state == DLCState.Accepted)
        assert(accept.fundingInputs.nonEmpty)
        assert(
          accept.fundingInputs
            .map(_.output.value)
            .sum >= accept.totalCollateral)
        assert(
          accept.totalCollateral == offer.contractInfo.max - offer.totalCollateral)
        assert(accept.changeAddress.value.nonEmpty)
      }

      sign <- walletA.signDLC(accept)
      dlcA2Opt <- walletA.dlcDAO.read(paramHash)
      _ = {
        assert(dlcA2Opt.isDefined)
        assert(dlcA2Opt.get.state == DLCState.Signed)
        assert(sign.fundingSigs.length == offerData.fundingInputs.size)
      }

      dlcDb <- walletB.addDLCSigs(sign)
      _ = assert(dlcDb.state == DLCState.Signed)
      outcomeSigs <- walletB.dlcSigsDAO.findByParamHash(offer.paramHash)

      refundSigsA <-
        walletA.dlcRefundSigDAO
          .findByParamHash(paramHash)
          .map(_.map(_.refundSig))
      refundSigsB <-
        walletB.dlcRefundSigDAO
          .findByParamHash(paramHash)
          .map(_.map(_.refundSig))

      walletAChange <- walletA.addressDAO.read(offer.changeAddress)
      walletAFinal <- walletA.addressDAO.read(offer.pubKeys.payoutAddress)

      walletBChange <- walletB.addressDAO.read(accept.changeAddress)
      walletBFinal <- walletB.addressDAO.read(accept.pubKeys.payoutAddress)

    } yield {
      assert(dlcDb.contractIdOpt.get == sign.contractId)

      assert(refundSigsA.size == 2)
      assert(refundSigsA.forall(refundSigsB.contains))

      assert(sign.cetSigs.outcomeSigs.forall(sig =>
        outcomeSigs.exists(dbSig => (dbSig.outcome, dbSig.signature) == sig)))

      // Test that the Addresses are in the wallet's database
      assert(walletAChange.isDefined)
      assert(walletAFinal.isDefined)
      assert(walletBChange.isDefined)
      assert(walletBFinal.isDefined)
    }
  }

  it must "correctly negotiate a dlc" in {
    fundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      testNegotiate(fundedDLCWallets, DLCWalletUtil.sampleDLCOffer)
  }

  it must "correctly negotiate a dlc with a multi-nonce oracle info" in {
    fundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      testNegotiate(fundedDLCWallets, DLCWalletUtil.sampleMultiNonceDLCOffer)
  }

  it must "correctly negotiate a dlc using TLVs" in {
    fundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = fundedDLCWallets._1.wallet
      val walletB = fundedDLCWallets._2.wallet

      val offerData = DLCWalletUtil.sampleDLCOffer

      for {
        offer <- walletA.createDLCOffer(
          offerData.oracleInfo,
          offerData.contractInfo.toTLV,
          offerData.totalCollateral,
          Some(offerData.feeRate),
          offerData.timeouts.contractMaturity.toUInt32,
          offerData.timeouts.contractTimeout.toUInt32
        )
        paramHash = offer.paramHash
        dlcA1Opt <- walletA.dlcDAO.read(paramHash)
        _ = {
          assert(dlcA1Opt.isDefined)
          assert(dlcA1Opt.get.state == DLCState.Offered)
          assert(offer.oracleInfo == offerData.oracleInfo)
          assert(offer.contractInfo == offerData.contractInfo)
          assert(offer.totalCollateral == offerData.totalCollateral)
          assert(offer.feeRate == offerData.feeRate)
          assert(offer.timeouts == offerData.timeouts)
          assert(offer.fundingInputs.nonEmpty)
          assert(offer.changeAddress.value.nonEmpty)
        }

        accept <- walletB.acceptDLCOffer(offer.toTLV)
        dlcB1Opt <- walletB.dlcDAO.read(paramHash)
        _ = {
          assert(dlcB1Opt.isDefined)
          assert(dlcB1Opt.get.state == DLCState.Accepted)
          assert(accept.fundingInputs.nonEmpty)
          assert(
            accept.fundingInputs
              .map(_.output.value)
              .sum >= accept.totalCollateral)
          assert(
            accept.totalCollateral == offer.contractInfo.max - offer.totalCollateral)
          assert(accept.changeAddress.value.nonEmpty)
        }

        sign <- walletA.signDLC(accept.toTLV)
        dlcA2Opt <- walletA.dlcDAO.read(paramHash)
        _ = {
          assert(dlcA2Opt.isDefined)
          assert(dlcA2Opt.get.state == DLCState.Signed)
          assert(sign.fundingSigs.length == offerData.fundingInputs.size)
        }

        dlcDb <- walletB.addDLCSigs(sign.toTLV)
        _ = assert(dlcDb.state == DLCState.Signed)
        outcomeSigs <- walletB.dlcSigsDAO.findByParamHash(offer.paramHash)

        refundSigsA <-
          walletA.dlcRefundSigDAO
            .findByParamHash(paramHash)
            .map(_.map(_.refundSig))
        refundSigsB <-
          walletB.dlcRefundSigDAO
            .findByParamHash(paramHash)
            .map(_.map(_.refundSig))

        walletAChange <- walletA.addressDAO.read(offer.changeAddress)
        walletAFinal <- walletA.addressDAO.read(offer.pubKeys.payoutAddress)

        walletBChange <- walletB.addressDAO.read(accept.changeAddress)
        walletBFinal <- walletB.addressDAO.read(accept.pubKeys.payoutAddress)

      } yield {
        assert(dlcDb.contractIdOpt.get == sign.contractId)

        assert(refundSigsA.size == 2)
        assert(refundSigsA.forall(refundSigsB.contains))

        assert(sign.cetSigs.outcomeSigs.forall(sig =>
          outcomeSigs.exists(dbSig => (dbSig.outcome, dbSig.signature) == sig)))

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

  it must "setup and execute with lloyd's example" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      val winStr = "Democrat_win"
      val loseStr = "Republican_win"
      val drawStr = "other"

      val betSize = 10000

      lazy val contractInfo: ContractInfo = {
        val winBytes = CryptoUtil.serializeForHash(winStr)
        val loseBytes = CryptoUtil.serializeForHash(loseStr)
        val drawBytes = CryptoUtil.serializeForHash(drawStr)

        ContractInfo(
          BigSizeUInt.calcFor(winBytes).bytes ++
            winBytes ++
            Satoshis(betSize).bytes ++
            BigSizeUInt.calcFor(loseBytes).bytes ++
            loseBytes ++
            Satoshis.zero.bytes ++
            BigSizeUInt.calcFor(drawBytes).bytes ++
            drawBytes ++
            Satoshis(betSize / 2).bytes)
      }

      val oraclePubKey = SchnorrPublicKey(
        "156c7d1c7922f0aa1168d9e21ac77ea88bbbe05e24e70a08bbe0519778f2e5da")
      val oracleNonce = SchnorrNonce(
        "ea3a68d8749b81682513b0479418d289d17e24d4820df2ce979f1a56a63ca525")
      val attestation = FieldElement(
        "77a5aabd716936411bbe19219bd0b261fae8f0524367268feb264e0a3b215766")

      val oracleInfo = SingleNonceOracleInfo(oraclePubKey, oracleNonce)

      val offerData = DLCOffer(
        OracleAndContractInfo(oracleInfo, contractInfo),
        dummyDLCKeys,
        Satoshis(5000),
        Vector(dummyFundingInputs.head),
        dummyAddress,
        SatoshisPerVirtualByte(Satoshis(3)),
        dummyTimeouts
      )

      val oracleSig = SchnorrDigitalSignature(oracleNonce, attestation)

      val paramHash = DLCMessage.calcParamHash(offerData.oracleInfo,
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
          assert(
            accept.totalCollateral == offer.contractInfo.max - offer.totalCollateral)
          assert(accept.changeAddress.value.nonEmpty)
        }

        sign <- walletA.signDLC(accept)
        _ = {
          assert(sign.fundingSigs.length == offerData.fundingInputs.size)
        }

        dlcDb <- walletB.addDLCSigs(sign)
        outcomeSigs <- walletB.dlcSigsDAO.findByParamHash(offer.paramHash)

        refundSigsA <-
          walletA.dlcRefundSigDAO
            .findByParamHash(paramHash)
            .map(_.map(_.refundSig))
        refundSigsB <-
          walletB.dlcRefundSigDAO
            .findByParamHash(paramHash)
            .map(_.map(_.refundSig))

        walletAChange <- walletA.addressDAO.read(offer.changeAddress)
        walletAFinal <- walletA.addressDAO.read(offer.pubKeys.payoutAddress)

        walletBChange <- walletB.addressDAO.read(accept.changeAddress)
        walletBFinal <- walletB.addressDAO.read(accept.pubKeys.payoutAddress)

        _ = {
          assert(dlcDb.contractIdOpt.get == sign.contractId)

          assert(refundSigsA.size == 2)
          assert(refundSigsA.forall(refundSigsB.contains))

          assert(sign.cetSigs.outcomeSigs.forall(sig =>
            outcomeSigs.exists(dbSig =>
              (dbSig.outcome, dbSig.signature) == sig)))
          // Test that the Addresses are in the wallet's database
          assert(walletAChange.isDefined)
          assert(walletAFinal.isDefined)
          assert(walletBChange.isDefined)
          assert(walletBFinal.isDefined)
        }

        func =
          (wallet: DLCWallet) => wallet.executeDLC(sign.contractId, oracleSig)
        result <- dlcExecutionTest(dlcA = walletA,
                                   dlcB = walletB,
                                   asInitiator = true,
                                   func = func,
                                   expectedOutputs = 1)
      } yield assert(result)
  }
}
