package org.bitcoins.dlc.wallet

import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.models.DLCMessage.DLCOffer
import org.bitcoins.core.protocol.dlc.models.DLCStatus.{
  Claimed,
  Refunded,
  RemoteClaimed
}
import org.bitcoins.core.protocol.dlc.models.{
  DLCState,
  DisjointUnionContractInfo,
  SingleContractInfo
}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.scalatest.FutureOutcome

import scala.concurrent.Future

class DLCExecutionTest extends BitcoinSDualWalletTest {
  override type FixtureParam = (InitializedDLCWallet, InitializedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test, DLCWalletUtil.sampleContractOraclePair)
  }

  behavior of "DLCWallet"

  it must "get the correct funding transaction" in { wallets =>
    val dlcA = wallets._1.wallet
    val dlcB = wallets._2.wallet

    for {
      contractId <- getContractId(dlcA)
      status <- getDLCStatus(dlcA)
      dlcId = status.dlcId
      offerOpt <- dlcA.dlcOfferDAO.findByDLCId(dlcId)
      acceptOpt <- dlcB.dlcAcceptDAO.findByDLCId(dlcId)

      inputsA <- dlcA.dlcInputsDAO.findByDLCId(dlcId)
      inputsB <- dlcB.dlcInputsDAO.findByDLCId(dlcId)

      fundingTx <- dlcB.getDLCFundingTx(contractId)
    } yield {
      assert(offerOpt.isDefined)
      assert(acceptOpt.isDefined)

      val offer = offerOpt.get
      val accept = acceptOpt.get

      val comparableInputsA = inputsA
        .sortBy(_.outPoint.hex)
        .map(_.copy(redeemScriptOpt = None, witnessScriptOpt = None))
      val comparableInputsB =
        inputsB
          .sortBy(_.outPoint.hex)
          .map(
            _.copy(redeemScriptOpt = None, witnessScriptOpt = None)
          ) // initiator will not have funding sigs

      assert(comparableInputsA == comparableInputsB)

      val fundingTxOutpoints = fundingTx.inputs.map(_.previousOutput)
      val outpointsA = inputsA.map(_.outPoint)
      val outpointsB = inputsB.map(_.outPoint)

      assert(fundingTxOutpoints.diff(outpointsA ++ outpointsB).isEmpty)

      assert(fundingTx.outputs.size == 3)
      assert(
        fundingTx.outputs.exists(
          _.scriptPubKey == offer.changeAddress.scriptPubKey))
      assert(
        fundingTx.outputs.exists(
          _.scriptPubKey == accept.changeAddress.scriptPubKey))
      assert(ScriptInterpreter.checkTransaction(fundingTx))

      val fundingTxPrevOutputRefs = inputsA.map(_.toOutputReference) ++ inputsB
        .map(_.toOutputReference)

      val prevOutputMap =
        fundingTxPrevOutputRefs.map(ref => ref.outPoint -> ref.output).toMap

      val fundingTxVerify = fundingTx.inputs.zipWithIndex.forall {
        case (input, index) =>
          val output = fundingTxPrevOutputRefs
            .find(_.outPoint == input.previousOutput)
            .get
            .output
          verifyInput(fundingTx, index, output, prevOutputMap)
      }
      assert(fundingTxVerify)
    }
  }

  it must "do a unilateral close as the initiator" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      status <- getDLCStatus(wallets._1.wallet)
      (sig, _) = {
        status.contractInfo match {
          case single: SingleContractInfo =>
            DLCWalletUtil.getSigs(single)
          case disjoint: DisjointUnionContractInfo =>
            sys.error(
              s"Cannot retrieve sigs for disjoint union contract, got=$disjoint")
        }
      }
      func = (wallet: DLCWallet) =>
        wallet.executeDLC(contractId, sig).map(_.get)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)
      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      dlcId = status.dlcId
      statusAOpt <- wallets._1.wallet.findDLC(dlcId)
      statusBOpt <- wallets._2.wallet.findDLC(dlcId)
      _ = {
        (statusAOpt, statusBOpt) match {
          case (Some(statusA: Claimed), Some(statusB: RemoteClaimed)) =>
            assert(statusA.oracleOutcome == statusB.oracleOutcome)
            assert(statusA.oracleSigs == Vector(statusB.oracleSig))
          case (_, _) => fail()
        }
      }
    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.Claimed)
          assert(dlcB.state == DLCState.RemoteClaimed)
        case (_, _) => fail()
      }
    }
  }

  it must "do a unilateral close as the recipient" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      status <- getDLCStatus(wallets._2.wallet)
      (_, sig) = {
        status.contractInfo match {
          case single: SingleContractInfo =>
            DLCWalletUtil.getSigs(single)
          case disjoint: DisjointUnionContractInfo =>
            sys.error(
              s"Cannot retrieve sigs for disjoint union contract, got=$disjoint")
        }
      }
      func = (wallet: DLCWallet) =>
        wallet.executeDLC(contractId, sig).map(_.get)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = false,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      dlcId = status.dlcId

      statusAOpt <- wallets._1.wallet.findDLC(dlcId)
      statusBOpt <- wallets._2.wallet.findDLC(dlcId)

      _ = {
        (statusAOpt, statusBOpt) match {
          case (Some(statusA: RemoteClaimed), Some(statusB: Claimed)) =>
            assert(statusA.oracleOutcome == statusB.oracleOutcome)
            assert(Vector(statusA.oracleSig) == statusB.oracleSigs)
          case (_, _) => fail()
        }
      }
    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.RemoteClaimed)
          assert(dlcB.state == DLCState.Claimed)
        case (_, _) => fail()
      }
    }
  }

  it must "execute a DLC twice and get the same transaction" in { wallets =>
    val wallet = wallets._1.wallet
    for {
      contractId <- getContractId(wallet)
      status <- getDLCStatus(wallet)
      (sig, _) = {
        status.contractInfo match {
          case single: SingleContractInfo =>
            DLCWalletUtil.getSigs(single)
          case disjoint: DisjointUnionContractInfo =>
            sys.error(
              s"Cannot retrieve sigs for disjoint union contract, got=$disjoint")
        }
      }

      tx1 <- wallet.executeDLC(contractId, sig)
      tx2 <- wallet.executeDLC(contractId, sig)
    } yield assert(tx1 == tx2)
  }

  it must "execute a losing dlc" in { wallets =>
    val dlcA = wallets._1.wallet

    for {
      contractId <- getContractId(wallets._1.wallet)
      status <- getDLCStatus(dlcA)
      // use dlcB winning sigs
      (_, sig) = {
        status.contractInfo match {
          case single: SingleContractInfo =>
            DLCWalletUtil.getSigs(single)
          case disjoint: DisjointUnionContractInfo =>
            sys.error(
              s"Cannot retrieve sigs for disjoint union contract, got=$disjoint")
        }
      }

      func = (wallet: DLCWallet) =>
        wallet.executeDLC(contractId, sig).map(_.get)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      dlcId = status.dlcId

      statusAOpt <- wallets._1.wallet.findDLC(dlcId)
      statusBOpt <- wallets._2.wallet.findDLC(dlcId)

      _ = {
        (statusAOpt, statusBOpt) match {
          case (Some(statusA: Claimed), Some(statusB: RemoteClaimed)) =>
            assert(statusA.oracleOutcome == statusB.oracleOutcome)
            assert(statusA.oracleSigs == Vector(statusB.oracleSig))
          case (_, _) => fail()
        }
      }
    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.Claimed)
          assert(dlcB.state == DLCState.RemoteClaimed)
        case (_, _) => fail()
      }
    }
  }

  it must "do a refund on a dlc as the initiator" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      status <- getDLCStatus(wallets._1.wallet)
      func = (wallet: DLCWallet) => wallet.executeDLCRefund(contractId)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 2)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      dlcId = status.dlcId

      statusAOpt <- wallets._1.wallet.findDLC(dlcId)
      statusBOpt <- wallets._2.wallet.findDLC(dlcId)

      _ = {
        (statusAOpt, statusBOpt) match {
          case (Some(statusA: Refunded), Some(statusB: Refunded)) =>
            assert(statusA.closingTxId == statusB.closingTxId)
          case (_, _) => fail()
        }
      }
    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.Refunded)
          assert(dlcB.state == DLCState.Refunded)
        case (_, _) => fail()
      }
    }
  }

  it must "do a refund on a dlc as the recipient" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      status <- getDLCStatus(wallets._1.wallet)
      func = (wallet: DLCWallet) => wallet.executeDLCRefund(contractId)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = false,
                                 func = func,
                                 expectedOutputs = 2)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      dlcId = status.dlcId

      statusAOpt <- wallets._1.wallet.findDLC(dlcId)
      statusBOpt <- wallets._2.wallet.findDLC(dlcId)

      _ = {
        (statusAOpt, statusBOpt) match {
          case (Some(statusA: Refunded), Some(statusB: Refunded)) =>
            assert(statusA.closingTxId == statusB.closingTxId)
          case (_, _) => fail()
        }
      }
    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.Refunded)
          assert(dlcB.state == DLCState.Refunded)
        case (_, _) => fail()
      }
    }
  }

  it must "fail to execute with an empty vec of sigs" in { wallets =>
    val dlcA = wallets._1.wallet

    val executeDLCForceCloseF = for {
      contractId <- getContractId(wallets._1.wallet)

      tx <- dlcA.executeDLC(contractId, Vector.empty[OracleAttestmentTLV])
    } yield tx

    recoverToSucceededIf[IllegalArgumentException](executeDLCForceCloseF)
  }

  it must "create 2 offers with the same contract info" in { wallets =>
    //test for: https://github.com/bitcoin-s/bitcoin-s/issues/3127
    val walletA = wallets._1.wallet

    //https://test.oracle.suredbits.com/contract/enum/75b08299654dca23b80cf359db6afb6cfd6e55bc898b5397d3c0fe796dfc13f0/12fb3e5f091086329ed0d2a12c3fcfa80111a36ef3fc1ac9c2567076a57d6a73
    val contractInfo = ContractInfoV0TLV.fromHex(
      "fdd82eeb00000000000186a0fda71026030359455300000000000186a0024e4f0000000000000000056f746865720000000000000000fda712b5fdd824b1596ec40d0dae3fdf54d9795ad51ec069970c6863a02d244663d39fd6bedadc0070349e1ba2e17583ee2d1cb3ae6fffaaa1c45039b61c5c4f1d0d864221c461745d1bcfab252c6dd9edd7aea4c5eeeef138f7ff7346061ea40143a9f5ae80baa9fdd8224d0001fa5b84283852400b21a840d5d5ca1cc31867c37326ad521aa50bebf3df4eea1a60b03280fdd8060f000303594553024e4f056f74686572135465746865722d52657365727665732d363342")
    val announcement =
      contractInfo.oracleInfo.asInstanceOf[OracleInfoV0TLV].announcement
    val feeRateOpt = Some(SatoshisPerVirtualByte(Satoshis.one))
    val totalCollateral = Satoshis(50000)

    //helper method to make an offer
    def makeOffer(): Future[DLCOffer] = {
      walletA.createDLCOffer(
        contractInfoTLV = contractInfo,
        collateral = totalCollateral,
        feeRateOpt = feeRateOpt,
        locktime = UInt32.zero,
        refundLT = UInt32.one,
        peerAddressOpt = None,
        externalPayoutAddressOpt = None,
        externalChangeAddressOpt = None
      )
    }

    //simply try to make 2 offers with the same contract info
    //if this works, we are good
    for {
      _ <- makeOffer()
      announcementVec1 <- walletA.announcementDAO.findByAnnouncementSignatures(
        Vector(announcement.announcementSignature))
      _ = assert(announcementVec1.length == 1,
                 s"Got length=${announcementVec1.length}")
      _ <- makeOffer()
      announcementVec2 <- walletA.announcementDAO.findByAnnouncementSignatures(
        Vector(announcement.announcementSignature))
      _ = assert(announcementVec2.length == 1,
                 s"Got length=${announcementVec2.length}")
    } yield succeed
  }

  it must "be able to construct an offer of the same contract info of a closed DLC" in {
    wallets =>
      val walletA = wallets._1.wallet
      val walletB = wallets._2.wallet

      for {
        contractId <- getContractId(walletA)
        status <- getDLCStatus(walletB)
        (_, sig) = {
          status.contractInfo match {
            case single: SingleContractInfo =>
              DLCWalletUtil.getSigs(single)
            case disjoint: DisjointUnionContractInfo =>
              sys.error(
                s"Cannot retrieve sigs for disjoint union contract, got=$disjoint")
          }
        }
        func = (wallet: DLCWallet) =>
          wallet.executeDLC(contractId, sig).map(_.get)

        result <- dlcExecutionTest(wallets = wallets,
                                   asInitiator = true,
                                   func = func,
                                   expectedOutputs = 1)
        _ = assert(result)

        _ <- walletA.createDLCOffer(status.contractInfo,
                                    status.localCollateral.satoshis,
                                    None,
                                    UInt32.zero,
                                    UInt32.one,
                                    None,
                                    None,
                                    None)

        _ <- walletA.listDLCs()
      } yield succeed
  }

  it must "throw an exception for a enum contract when do not have all the oracle signatures/outcomes" in {
    wallets =>
      val walletA = wallets._1.wallet
      val resultF = for {
        contractId <- getContractId(walletA)
        status <- getDLCStatus(walletA)
        (goodAttestment, _) = {
          status.contractInfo match {
            case single: SingleContractInfo =>
              DLCWalletUtil.getSigs(single)
            case disjoint: DisjointUnionContractInfo =>
              sys.error(
                s"Cannot retrieve sigs for disjoint union contract, got=$disjoint")
          }
        }
        //purposefully drop these
        //we cannot drop just a sig, or just an outcome because
        //of invariants in OracleAttestmentV0TLV
        badSigs = goodAttestment.sigs.dropRight(1)
        badOutcomes = goodAttestment.outcomes.dropRight(1)
        badAttestment = OracleAttestmentV0TLV(eventId = goodAttestment.eventId,
                                              publicKey =
                                                goodAttestment.publicKey,
                                              sigs = badSigs,
                                              outcomes = badOutcomes)
        func = (wallet: DLCWallet) =>
          wallet.executeDLC(contractId, badAttestment).map(_.get)

        result <- dlcExecutionTest(wallets = wallets,
                                   asInitiator = true,
                                   func = func,
                                   expectedOutputs = 1)
      } yield assert(result)

      recoverToSucceededIf[IllegalArgumentException](resultF)
  }

  it must "throw an exception when you try to execute a DLC in the SIGNED state" in {
    wallets =>
      val walletA = wallets._1.wallet
      val walletB = wallets._2.wallet
      val contractOraclePair = DLCWalletUtil.sampleContractOraclePair
      val amt = Bitcoins.one
      val contractInfo = SingleContractInfo(amt.satoshis, contractOraclePair)
      val resultF = for {
        offer <- walletA.createDLCOffer(
          contractInfo = contractInfo,
          collateral = half,
          feeRateOpt = Some(SatoshisPerVirtualByte.fromLong(10)),
          locktime = dummyTimeouts.contractMaturity.toUInt32,
          refundLocktime = dummyTimeouts.contractTimeout.toUInt32,
          peerAddressOpt = None,
          externalPayoutAddressOpt = None,
          externalChangeAddressOpt = None
        )
        accept <- walletB.acceptDLCOffer(offer, None, None, None)
        sign <- walletA.signDLC(accept)
        contractId = sign.contractId
        (_, sig) = DLCWalletUtil.getSigs(contractInfo)
        _ <- walletA.executeDLC(contractId, sig)
      } yield succeed

      recoverToSucceededIf[RuntimeException](resultF)
  }
}
