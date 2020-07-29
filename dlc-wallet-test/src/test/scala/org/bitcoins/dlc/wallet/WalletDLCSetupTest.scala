package org.bitcoins.dlc.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script.P2WPKHWitnessV0
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.crypto._
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future
import scala.reflect.ClassTag

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

      assert(sign.cetSigs.outcomeSigs.forall { case (outcome, sig) =>
        outcomeSigs.exists(dbSig =>
          (dbSig.sigPoint, dbSig.signature) == ((outcome, sig)))
      })

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

        assert(sign.cetSigs.outcomeSigs.forall { case (outcome, sig) =>
          outcomeSigs.exists(dbSig =>
            (dbSig.sigPoint, dbSig.signature) == ((outcome, sig)))
        })

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
        offerData.contractInfo,
        offerData.totalCollateral,
        Some(offerData.feeRate),
        offerData.timeouts.contractMaturity.toUInt32,
        offerData.timeouts.contractTimeout.toUInt32
      )

      accept <- walletB.acceptDLCOffer(offer)
    } yield accept
  }

  def testDLCSignVerification[E <: Exception](
      walletA: DLCWallet,
      walletB: DLCWallet,
      makeDLCSignInvalid: DLCSign => DLCSign)(implicit
      classTag: ClassTag[E]): Future[Assertion] = {
    val failedAddSigsF = for {
      sign <- getDLCReadyToAddSigs(walletA, walletB)
      invalidSign = makeDLCSignInvalid(sign)
      dlcDb <- walletB.addDLCSigs(invalidSign)
    } yield dlcDb

    recoverToSucceededIf[E](failedAddSigsF)
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

  it must "fail to add dlc funding sigs that do not correspond to the DLC" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      testDLCSignVerification[NoSuchElementException](
        walletA,
        walletB,
        (sign: DLCSign) =>
          sign.copy(fundingSigs = DLCWalletUtil.dummyFundingSignatures)
      )
  }

  it must "fail to add dlc funding sigs that are invalid" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      testDLCSignVerification[IllegalArgumentException](
        walletA,
        walletB,
        (sign: DLCSign) =>
          sign.copy(fundingSigs = FundingSignatures(
            sign.fundingSigs
              .map(_.copy(_2 = P2WPKHWitnessV0(ECPublicKey.freshPublicKey)))
              .toVector))
      )
  }

  it must "fail to add dlc cet sigs that are invalid" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      testDLCSignVerification[IllegalArgumentException](
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

      testDLCSignVerification[IllegalArgumentException](
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

  it must "cancel an offered DLC" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet

      val offerData: DLCOffer = DLCWalletUtil.sampleDLCOffer

      for {
        oldBalance <- walletA.getBalance()
        oldReserved <- walletA.spendingInfoDAO.findByTxoState(TxoState.Reserved)
        _ = assert(oldReserved.isEmpty)

        offer <- walletA.createDLCOffer(
          offerData.contractInfo,
          offerData.totalCollateral,
          Some(offerData.feeRate),
          offerData.timeouts.contractMaturity.toUInt32,
          offerData.timeouts.contractTimeout.toUInt32
        )

        _ <- walletA.cancelDLC(offer.paramHash)

        balance <- walletA.getBalance()
        reserved <- walletA.spendingInfoDAO.findByTxoState(TxoState.Reserved)
        dlcOpt <- walletA.findDLC(offer.paramHash)
      } yield {
        assert(balance == oldBalance)
        assert(reserved.isEmpty)
        assert(dlcOpt.isEmpty)
      }
  }

  it must "cancel an accepted DLC" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      val offerData: DLCOffer = DLCWalletUtil.sampleDLCOffer

      for {
        oldBalance <- walletB.getBalance()
        oldReserved <- walletB.spendingInfoDAO.findByTxoState(TxoState.Reserved)
        _ = assert(oldReserved.isEmpty)

        offer <- walletA.createDLCOffer(
          offerData.contractInfo,
          offerData.totalCollateral,
          Some(offerData.feeRate),
          offerData.timeouts.contractMaturity.toUInt32,
          offerData.timeouts.contractTimeout.toUInt32
        )
        _ <- walletB.acceptDLCOffer(offer)

        _ <- walletB.cancelDLC(offer.paramHash)

        balance <- walletB.getBalance()
        reserved <- walletB.spendingInfoDAO.findByTxoState(TxoState.Reserved)
        dlcOpt <- walletB.findDLC(offer.paramHash)
      } yield {
        assert(balance == oldBalance)
        assert(reserved.isEmpty)
        assert(dlcOpt.isEmpty)
      }
  }

  it must "fail to cancel a signed DLC" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      val offerData: DLCOffer = DLCWalletUtil.sampleDLCOffer

      for {
        offer <- walletA.createDLCOffer(
          offerData.contractInfo,
          offerData.totalCollateral,
          Some(offerData.feeRate),
          offerData.timeouts.contractMaturity.toUInt32,
          offerData.timeouts.contractTimeout.toUInt32
        )
        accept <- walletB.acceptDLCOffer(offer)
        sign <- walletA.signDLC(accept)
        _ <- walletB.addDLCSigs(sign)

        _ <- recoverToSucceededIf[IllegalArgumentException](
          walletA.cancelDLC(offer.paramHash))

        _ <- recoverToSucceededIf[IllegalArgumentException](
          walletB.cancelDLC(offer.paramHash))
      } yield succeed
  }

  it must "setup and execute with oracle example" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      val winStr = "Democrat_win"
      val loseStr = "Republican_win"
      val drawStr = "other"

      val betSize = 10000

      val contractDescriptor: EnumContractDescriptor =
        EnumContractDescriptor.fromStringVec(
          Vector(winStr -> Satoshis(betSize),
                 loseStr -> Satoshis.zero,
                 drawStr -> Satoshis(betSize / 2)))

      val oracleInfo = EnumSingleOracleInfo(OracleAnnouncementTLV(
        "fdd824b4caaec7479cc9d37003f5add6504d035054ffeac8637a990305a45cfecc1062044c3f68b45318f57e41c4544a4a950c0744e2a80854349a3426b00ad86da5090b9e942dc6df2ae87f007b45b0ccd63e6c354d92c4545fc099ea3e137e54492d1efdd822500001a6a09c7c83c50b34f9db560a2e14fef2eab5224c15b18c7114331756364bfce65ffe3800fdd8062400030c44656d6f637261745f77696e0e52657075626c6963616e5f77696e056f746865720161"))

      val offerData = DLCOffer(
        ContractInfo(contractDescriptor, oracleInfo),
        dummyDLCKeys,
        Satoshis(5000),
        Vector(dummyFundingInputs.head),
        dummyAddress,
        payoutSerialId = UInt64.zero,
        changeSerialId = UInt64.one,
        fundOutputSerialId = UInt64.max,
        SatoshisPerVirtualByte(Satoshis(3)),
        dummyTimeouts
      )

      val oracleSig = SchnorrDigitalSignature(
        "a6a09c7c83c50b34f9db560a2e14fef2eab5224c15b18c7114331756364bfce6c59736cdcfe1e0a89064f846d5dbde0902f82688dde34dc1833965a60240f287")

      val sig = OracleSignatures(oracleInfo, Vector(oracleSig))

      val paramHash =
        DLCMessage.calcParamHash(offerData.contractInfo, offerData.timeouts)

      for {
        offer <- walletA.createDLCOffer(
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

          assert(sign.cetSigs.outcomeSigs.forall { case (outcome, sig) =>
            outcomeSigs.exists(dbSig =>
              (dbSig.sigPoint, dbSig.signature) == ((outcome, sig)))
          })
          // Test that the Addresses are in the wallet's database
          assert(walletAChange.isDefined)
          assert(walletAFinal.isDefined)
          assert(walletBChange.isDefined)
          assert(walletBFinal.isDefined)
        }

        tx <- walletB.broadcastDLCFundingTx(sign.contractId)
        _ <- walletA.processTransaction(tx, None)

        func = (wallet: DLCWallet) => wallet.executeDLC(sign.contractId, sig)
        result <- dlcExecutionTest(dlcA = walletA,
                                   dlcB = walletB,
                                   asInitiator = true,
                                   func = func,
                                   expectedOutputs = 1)
      } yield assert(result)
  }
}
