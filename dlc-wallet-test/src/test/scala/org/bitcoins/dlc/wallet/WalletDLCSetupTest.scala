package org.bitcoins.dlc.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script.P2WPKHWitnessV0
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.crypto._
import org.bitcoins.dlc.wallet.internal.DLCDataManagement
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
    val walletADLCManagement = DLCDataManagement(walletA.dlcWalletDAOs)
    val walletBDLCManagement = DLCDataManagement(walletB.dlcWalletDAOs)
    for {
      offer <- walletA.createDLCOffer(
        offerData.contractInfo,
        offerData.totalCollateral,
        Some(offerData.feeRate),
        offerData.timeouts.contractMaturity.toUInt32,
        offerData.timeouts.contractTimeout.toUInt32
      )
      dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))
      dlcA1Opt <- walletA.dlcDAO.read(dlcId)
      find1 <- walletA.findDLC(dlcId)
      _ = {
        assert(find1.isDefined)
        assert(dlcA1Opt.get.state == DLCState.Offered)
        assert(offer.oracleInfos == offerData.oracleInfos)
        assert(offer.contractInfo == offerData.contractInfo)
        assert(offer.totalCollateral == offerData.totalCollateral)
        assert(offer.feeRate == offerData.feeRate)
        assert(offer.timeouts == offerData.timeouts)
        assert(offer.fundingInputs.nonEmpty)
        assert(offer.changeAddress.value.nonEmpty)
      }

      accept <- walletB.acceptDLCOffer(offer)
      dlcB1Opt <- walletB.dlcDAO.read(dlcId)
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
      dlcA2Opt <- walletA.dlcDAO.read(dlcId)
      _ = {
        assert(dlcA2Opt.isDefined)
        assert(dlcA2Opt.get.state == DLCState.Signed)
        assert(sign.fundingSigs.length == offer.fundingInputs.size)
      }

      dlcDb <- walletB.addDLCSigs(sign)
      _ = assert(dlcDb.state == DLCState.Signed)
      outcomeSigs <- walletB.dlcSigsDAO.findByDLCId(dlcId)

      refundSigsA <- walletA.dlcRefundSigDAO.findByDLCId(dlcId)
      refundSigsB <- walletB.dlcRefundSigDAO.findByDLCId(dlcId)

      walletAChange <- walletA.addressDAO.read(offer.changeAddress)
      walletAFinal <- walletA.addressDAO.read(offer.pubKeys.payoutAddress)

      walletBChange <- walletB.addressDAO.read(accept.changeAddress)
      walletBFinal <- walletB.addressDAO.read(accept.pubKeys.payoutAddress)

      (announcementsA, announcementDataA, nonceDbsA) <- walletADLCManagement
        .getDLCAnnouncementDbs(dlcDb.dlcId)
      announcementTLVsA = walletADLCManagement.getOracleAnnouncements(
        announcementsA,
        announcementDataA,
        nonceDbsA)

      (announcementsB, announcementDataB, nonceDbsB) <- walletADLCManagement
        .getDLCAnnouncementDbs(dlcDb.dlcId)
      announcementTLVsB = walletBDLCManagement.getOracleAnnouncements(
        announcementsB,
        announcementDataB,
        nonceDbsB)
    } yield {
      assert(dlcDb.contractIdOpt.get == sign.contractId)

      assert(refundSigsA.isDefined)
      assert(refundSigsB.isDefined)
      assert(refundSigsA.get.initiatorSig.isDefined)
      assert(refundSigsA.get.initiatorSig == refundSigsB.get.initiatorSig)
      assert(refundSigsA.get.accepterSig == refundSigsB.get.accepterSig)

      assert(sign.cetSigs.outcomeSigs.forall { case (outcome, sig) =>
        outcomeSigs.exists(dbSig =>
          (dbSig.sigPoint, dbSig.initiatorSig.get) == ((outcome, sig)))
      })

      assert(announcementTLVsA == announcementTLVsB)

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

  // This could happen inputs can end up in different orders when
  // using postgres or using different coin selection algos
  it must "correctly negotiate a dlc with reordered inputs" in {
    fundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      // construct a contract info that uses many inputs
      val totalCol = Bitcoins(11).satoshis
      val col = totalCol / Satoshis(2)

      val outcomes: Vector[(EnumOutcome, Satoshis)] =
        Vector(EnumOutcome(winStr) -> totalCol,
               EnumOutcome(loseStr) -> Satoshis.zero)

      val oraclePair: ContractOraclePair.EnumPair =
        ContractOraclePair.EnumPair(EnumContractDescriptor(outcomes),
                                    sampleOracleInfo)

      val contractInfo: ContractInfo = SingleContractInfo(totalCol, oraclePair)

      val offerData =
        sampleDLCOffer.copy(contractInfo = contractInfo,
                            totalCollateral = col.satoshis)

      val walletA = fundedDLCWallets._1.wallet
      val walletB = fundedDLCWallets._2.wallet

      def reorderInputDbs(
          wallet: DLCWallet,
          dlcId: Sha256Digest): Future[Unit] = {
        for {
          inputDbs <- wallet.dlcInputsDAO.findByDLCId(dlcId)
          _ <- wallet.dlcInputsDAO.deleteByDLCId(dlcId)
          _ <- wallet.dlcInputsDAO.createAll(inputDbs.reverse)
        } yield ()
      }

      for {
        offer <- walletA.createDLCOffer(
          offerData.contractInfo,
          offerData.totalCollateral,
          Some(offerData.feeRate),
          offerData.timeouts.contractMaturity.toUInt32,
          offerData.timeouts.contractTimeout.toUInt32
        )
        dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))

        accept <- walletB.acceptDLCOffer(offer)

        // reorder dlc inputs in wallets
        _ <- reorderInputDbs(walletA, dlcId)
        _ <- reorderInputDbs(walletB, dlcId)

        sign <- walletA.signDLC(accept)

        dlcDb <- walletB.addDLCSigs(sign)
      } yield assert(dlcDb.state == DLCState.Signed)
  }

  // This could happen inputs can end up in different orders when
  // using postgres or using different coin selection algos
  it must "correctly negotiate a dlc with tlvs & with reordered inputs" in {
    fundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      // construct a contract info that uses many inputs
      val totalCol = Bitcoins(11).satoshis
      val col = totalCol / Satoshis(2)

      val outcomes: Vector[(EnumOutcome, Satoshis)] =
        Vector(EnumOutcome(winStr) -> totalCol,
               EnumOutcome(loseStr) -> Satoshis.zero)

      val oraclePair: ContractOraclePair.EnumPair =
        ContractOraclePair.EnumPair(EnumContractDescriptor(outcomes),
                                    sampleOracleInfo)

      val contractInfo: ContractInfo = SingleContractInfo(totalCol, oraclePair)

      val offerData =
        sampleDLCOffer.copy(contractInfo = contractInfo,
                            totalCollateral = col.satoshis)

      val walletA = fundedDLCWallets._1.wallet
      val walletB = fundedDLCWallets._2.wallet

      def reorderInputDbs(
          wallet: DLCWallet,
          dlcId: Sha256Digest): Future[Unit] = {
        for {
          inputDbs <- wallet.dlcInputsDAO.findByDLCId(dlcId)
          _ <- wallet.dlcInputsDAO.deleteByDLCId(dlcId)
          _ <- wallet.dlcInputsDAO.createAll(inputDbs.reverse)
        } yield ()
      }

      for {
        offer <- walletA.createDLCOffer(
          offerData.contractInfo,
          offerData.totalCollateral,
          Some(offerData.feeRate),
          offerData.timeouts.contractMaturity.toUInt32,
          offerData.timeouts.contractTimeout.toUInt32
        )
        dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))

        accept <- walletB.acceptDLCOffer(offer.toTLV)

        // reorder dlc inputs in wallets
        _ <- reorderInputDbs(walletA, dlcId)
        _ <- reorderInputDbs(walletB, dlcId)

        sign <- walletA.signDLC(accept.toTLV)

        dlcDb <- walletB.addDLCSigs(sign.toTLV)
      } yield assert(dlcDb.state == DLCState.Signed)
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
        dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))
        dlcA1Opt <- walletA.dlcDAO.read(dlcId)
        _ = {
          assert(dlcA1Opt.isDefined)
          assert(dlcA1Opt.get.state == DLCState.Offered)
          assert(offer.oracleInfos == offerData.oracleInfos)
          assert(offer.contractInfo == offerData.contractInfo)
          assert(offer.totalCollateral == offerData.totalCollateral)
          assert(offer.feeRate == offerData.feeRate)
          assert(offer.timeouts == offerData.timeouts)
          assert(offer.fundingInputs.nonEmpty)
          assert(offer.changeAddress.value.nonEmpty)
        }

        accept <- walletB.acceptDLCOffer(offer.toTLV)
        dlcB1Opt <- walletB.dlcDAO.read(dlcId)
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
        dlcA2Opt <- walletA.dlcDAO.read(dlcId)
        _ = {
          assert(dlcA2Opt.isDefined)
          assert(dlcA2Opt.get.state == DLCState.Signed)
          assert(sign.fundingSigs.length == offer.fundingInputs.size)
        }

        dlcDb <- walletB.addDLCSigs(sign.toTLV)
        _ = assert(dlcDb.state == DLCState.Signed)
        outcomeSigs <- walletB.dlcSigsDAO.findByDLCId(dlcId)

        refundSigsA <- walletA.dlcRefundSigDAO.findByDLCId(dlcId)
        refundSigsB <- walletB.dlcRefundSigDAO.findByDLCId(dlcId)

        walletAChange <- walletA.addressDAO.read(offer.changeAddress)
        walletAFinal <- walletA.addressDAO.read(offer.pubKeys.payoutAddress)

        walletBChange <- walletB.addressDAO.read(accept.changeAddress)
        walletBFinal <- walletB.addressDAO.read(accept.pubKeys.payoutAddress)

      } yield {
        assert(dlcDb.contractIdOpt.get == sign.contractId)

        assert(refundSigsA.isDefined)
        assert(refundSigsB.isDefined)
        assert(refundSigsA.get.initiatorSig.isDefined)
        assert(refundSigsA.get.initiatorSig == refundSigsB.get.initiatorSig)
        assert(refundSigsA.get.accepterSig == refundSigsB.get.accepterSig)

        assert(sign.cetSigs.outcomeSigs.forall { case (outcome, sig) =>
          outcomeSigs.exists(dbSig =>
            (dbSig.sigPoint, dbSig.initiatorSig.get) == ((outcome, sig)))
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

  it must "fail to add its own sigs" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      for {
        sign <- getDLCReadyToAddSigs(walletA, walletB)
        _ <- recoverToSucceededIf[IllegalArgumentException](
          walletA.addDLCSigs(sign))
      } yield succeed
  }

  it must "fail to add dlc funding sigs that do not correspond to the DLC" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      testDLCSignVerification[IllegalArgumentException](
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
          sign.copy(cetSigs = CETSignatures(DLCWalletUtil.dummyOutcomeSigs)))
  }

  it must "fail to add an invalid dlc refund sig" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      testDLCSignVerification[IllegalArgumentException](
        walletA,
        walletB,
        (sign: DLCSign) => sign.copy(refundSig = DLCWalletUtil.dummyPartialSig))
  }

  it must "fail to sign dlc with cet sigs that are invalid" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      testDLCAcceptVerification(
        walletA,
        walletB,
        (accept: DLCAccept) =>
          accept.copy(cetSigs = CETSignatures(DLCWalletUtil.dummyOutcomeSigs)))
  }

  it must "fail to sign dlc with an invalid refund sig" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      testDLCAcceptVerification(
        walletA,
        walletB,
        (accept: DLCAccept) =>
          accept.copy(refundSig = DLCWalletUtil.dummyPartialSig))
  }

  it must "cancel an offered DLC" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet

      val offerData: DLCOffer = DLCWalletUtil.sampleDLCOffer

      val announcementTLVs =
        offerData.contractInfo.oracleInfos.head.singleOracleInfos
          .map(_.announcement)
      assert(announcementTLVs.size == 1)
      val announcementTLV = announcementTLVs.head

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

        dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))

        _ <- walletA.cancelDLC(dlcId)

        announcementData <- walletA.announcementDAO.findByPublicKey(
          announcementTLV.publicKey)
        nonceDbs <- walletA.oracleNonceDAO.findByAnnouncementIds(
          announcementData.map(_.id.get))

        balance <- walletA.getBalance()
        reserved <- walletA.spendingInfoDAO.findByTxoState(TxoState.Reserved)
        dlcOpt <- walletA.findDLC(dlcId)
      } yield {
        assert(balance == oldBalance)
        assert(reserved.isEmpty)
        assert(dlcOpt.isEmpty)

        // Check we persist the announcements
        assert(announcementData.nonEmpty)
        assert(nonceDbs.nonEmpty)
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

        dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))

        _ <- walletB.cancelDLC(dlcId)

        balance <- walletB.getBalance()
        reserved <- walletB.spendingInfoDAO.findByTxoState(TxoState.Reserved)
        dlcOpt <- walletB.findDLC(dlcId)
      } yield {
        assert(balance == oldBalance)
        assert(reserved.isEmpty)
        assert(dlcOpt.isEmpty)
      }
  }

  it must "cancel a signed DLC" in {
    FundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = FundedDLCWallets._1.wallet
      val walletB = FundedDLCWallets._2.wallet

      val offerData: DLCOffer = DLCWalletUtil.sampleDLCOffer

      for {
        oldBalanceA <- walletA.getBalance()
        oldReservedA <- walletA.spendingInfoDAO.findByTxoState(
          TxoState.Reserved)
        _ = assert(oldReservedA.isEmpty)

        oldBalanceB <- walletB.getBalance()
        oldReservedB <- walletB.spendingInfoDAO.findByTxoState(
          TxoState.Reserved)
        _ = assert(oldReservedB.isEmpty)

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

        dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))

        _ <- walletA.cancelDLC(dlcId)
        _ <- walletB.cancelDLC(dlcId)

        balanceA <- walletA.getBalance()
        reservedA <- walletA.spendingInfoDAO.findByTxoState(TxoState.Reserved)
        dlcAOpt <- walletA.findDLC(dlcId)

        balanceB <- walletB.getBalance()
        reservedB <- walletB.spendingInfoDAO.findByTxoState(TxoState.Reserved)
        dlcBOpt <- walletB.findDLC(dlcId)
      } yield {
        assert(balanceA == oldBalanceA)
        assert(reservedA.isEmpty)
        assert(dlcAOpt.isEmpty)

        assert(balanceB == oldBalanceB)
        assert(reservedB.isEmpty)
        assert(dlcBOpt.isEmpty)
      }
  }

  it must "fail to cancel a broadcasted DLC" in {
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

        tx <- walletB.broadcastDLCFundingTx(sign.contractId)
        // make sure other wallet sees it
        _ <- walletA.processTransaction(tx, None)

        dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))

        _ <- recoverToSucceededIf[IllegalArgumentException](
          walletA.cancelDLC(dlcId))

        _ <- recoverToSucceededIf[IllegalArgumentException](
          walletB.cancelDLC(dlcId))
      } yield succeed
  }

  it must "fail to refund a DLC that hasn't reached its timeout" in {
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
          UInt32.max
        )
        accept <- walletB.acceptDLCOffer(offer)
        sign <- walletA.signDLC(accept)
        _ <- walletB.addDLCSigs(sign)

        tx <- walletB.broadcastDLCFundingTx(sign.contractId)
        // make sure other wallet sees it
        _ <- walletA.processTransaction(tx, None)

        dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))

        _ <- recoverToSucceededIf[IllegalArgumentException](
          walletA.executeDLCRefund(sign.contractId))

        _ <- recoverToSucceededIf[IllegalArgumentException](
          walletB.executeDLCRefund(sign.contractId))
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
        DLCOfferTLV.currentVersionOpt,
        SingleContractInfo(contractDescriptor, oracleInfo),
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

      for {
        offer <- walletA.createDLCOffer(
          offerData.contractInfo,
          offerData.totalCollateral,
          Some(offerData.feeRate),
          offerData.timeouts.contractMaturity.toUInt32,
          offerData.timeouts.contractTimeout.toUInt32
        )
        _ = {
          assert(offer.oracleInfos == offerData.oracleInfos)
          assert(offer.contractInfo == offerData.contractInfo)
          assert(offer.totalCollateral == offerData.totalCollateral)
          assert(offer.feeRate == offerData.feeRate)
          assert(offer.timeouts == offerData.timeouts)
          assert(offer.fundingInputs.nonEmpty)
          assert(offer.changeAddress.value.nonEmpty)
        }

        dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))

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
        outcomeSigs <- walletB.dlcSigsDAO.findByDLCId(dlcId)

        refundSigsA <- walletA.dlcRefundSigDAO.findByDLCId(dlcId)
        refundSigsB <- walletB.dlcRefundSigDAO.findByDLCId(dlcId)

        walletAChange <- walletA.addressDAO.read(offer.changeAddress)
        walletAFinal <- walletA.addressDAO.read(offer.pubKeys.payoutAddress)

        walletBChange <- walletB.addressDAO.read(accept.changeAddress)
        walletBFinal <- walletB.addressDAO.read(accept.pubKeys.payoutAddress)

        _ = {
          assert(dlcDb.contractIdOpt.get == sign.contractId)

          assert(refundSigsA.isDefined)
          assert(refundSigsB.isDefined)
          assert(refundSigsA.get.initiatorSig.isDefined)
          assert(refundSigsA.get.initiatorSig == refundSigsB.get.initiatorSig)
          assert(refundSigsA.get.accepterSig == refundSigsB.get.accepterSig)

          assert(sign.cetSigs.outcomeSigs.forall { case (outcome, sig) =>
            outcomeSigs.exists(dbSig =>
              (dbSig.sigPoint, dbSig.initiatorSig.get) == ((outcome, sig)))
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
      } yield {
        assert(result)
      }
  }

  it must "accept 2 offers with the same oracle info" in { wallets =>
    val walletA = wallets._1.wallet
    val walletB = wallets._2.wallet

    //https://test.oracle.suredbits.com/contract/enum/75b08299654dca23b80cf359db6afb6cfd6e55bc898b5397d3c0fe796dfc13f0/12fb3e5f091086329ed0d2a12c3fcfa80111a36ef3fc1ac9c2567076a57d6a73
    val contractInfoA = ContractInfoV0TLV.fromHex(
      "fdd82eeb00000000000186a0fda71026030359455300000000000186a0024e4f0000000000000000056f746865720000000000000000fda712b5fdd824b1596ec40d0dae3fdf54d9795ad51ec069970c6863a02d244663d39fd6bedadc0070349e1ba2e17583ee2d1cb3ae6fffaaa1c45039b61c5c4f1d0d864221c461745d1bcfab252c6dd9edd7aea4c5eeeef138f7ff7346061ea40143a9f5ae80baa9fdd8224d0001fa5b84283852400b21a840d5d5ca1cc31867c37326ad521aa50bebf3df4eea1a60b03280fdd8060f000303594553024e4f056f74686572135465746865722d52657365727665732d363342")
    //https://test.oracle.suredbits.com/contract/enum/75b08299654dca23b80cf359db6afb6cfd6e55bc898b5397d3c0fe796dfc13f0/e5fb1dd68e51f5d735a0dd83ff88795bd7c959003a01e16c1ad08df3758de057
    val contractInfoB = ContractInfoV0TLV.fromHex(
      "fdd82eeb0000000000002710fda7102603035945530000000000000000024e4f0000000000002710056f746865720000000000000000fda712b5fdd824b1596ec40d0dae3fdf54d9795ad51ec069970c6863a02d244663d39fd6bedadc0070349e1ba2e17583ee2d1cb3ae6fffaaa1c45039b61c5c4f1d0d864221c461745d1bcfab252c6dd9edd7aea4c5eeeef138f7ff7346061ea40143a9f5ae80baa9fdd8224d0001fa5b84283852400b21a840d5d5ca1cc31867c37326ad521aa50bebf3df4eea1a60b03280fdd8060f000303594553024e4f056f74686572135465746865722d52657365727665732d363342")

    assert(contractInfoA.oracleInfo == contractInfoB.oracleInfo)

    val feeRateOpt = Some(SatoshisPerVirtualByte(Satoshis.one))
    val totalCollateral = Satoshis(5000)

    def makeOffer(contractInfo: ContractInfoV0TLV): Future[DLCOffer] = {
      walletA.createDLCOffer(contractInfoTLV = contractInfo,
                             collateral = totalCollateral,
                             feeRateOpt = feeRateOpt,
                             locktime = UInt32.zero,
                             refundLT = UInt32.one)
    }

    for {
      offerA <- makeOffer(contractInfoA)
      offerB <- makeOffer(contractInfoB)

      _ <- walletB.acceptDLCOffer(offerA)
      _ <- walletB.acceptDLCOffer(offerB)
    } yield succeed
  }

  it must "not be able to sign its own accept" in { wallets =>
    val walletA = wallets._1.wallet
    val walletB = wallets._2.wallet

    val offerData: DLCOffer = DLCWalletUtil.sampleDLCOffer

    for {
      offer <- walletA.createDLCOffer(
        offerData.contractInfo,
        offerData.totalCollateral,
        Some(offerData.feeRate),
        offerData.timeouts.contractMaturity.toUInt32,
        UInt32.max
      )
      accept <- walletB.acceptDLCOffer(offer)
      res <- recoverToSucceededIf[IllegalArgumentException](
        walletB.signDLC(accept))
    } yield res
  }

}
