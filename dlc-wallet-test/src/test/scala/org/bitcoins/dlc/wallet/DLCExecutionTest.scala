package org.bitcoins.dlc.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.models.DLCMessage.DLCOffer
import org.bitcoins.core.protocol.dlc.models.DLCStatus.{
  Claimed,
  Refunded,
  RemoteClaimed
}
import org.bitcoins.core.protocol.dlc.models.{
  ContractInfo,
  DLCState,
  EnumContractDescriptor,
  NumericContractDescriptor
}
import org.bitcoins.core.protocol.tlv.{
  ContractInfoV0TLV,
  OracleAttestmentTLV,
  OracleAttestmentV0TLV,
  OracleEventV0TLV,
  OracleInfoV0TLV
}
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.scalatest.FutureOutcome

import scala.concurrent.Future

class DLCExecutionTest extends BitcoinSDualWalletTest {
  type FixtureParam = (InitializedDLCWallet, InitializedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test, DLCWalletUtil.sampleContractOraclePair)
  }

  behavior of "DLCWallet"

  def getSigs(contractInfo: ContractInfo): (
      OracleAttestmentTLV,
      OracleAttestmentTLV) = {
    val desc: EnumContractDescriptor = contractInfo.contractDescriptor match {
      case desc: EnumContractDescriptor => desc
      case _: NumericContractDescriptor =>
        throw new IllegalArgumentException("Unexpected Contract Info")
    }

    // Get a hash that the initiator wins for
    val initiatorWinStr =
      desc
        .maxBy(_._2.toLong)
        ._1
        .outcome
    val initiatorWinSig = DLCWalletUtil.oraclePrivKey
      .schnorrSignWithNonce(CryptoUtil
                              .sha256DLCAttestation(initiatorWinStr)
                              .bytes,
                            DLCWalletUtil.kValue)

    // Get a hash that the recipient wins for
    val recipientWinStr =
      desc.find(_._2 == Satoshis.zero).get._1.outcome
    val recipientWinSig = DLCWalletUtil.oraclePrivKey
      .schnorrSignWithNonce(CryptoUtil
                              .sha256DLCAttestation(recipientWinStr)
                              .bytes,
                            DLCWalletUtil.kValue)

    val publicKey = DLCWalletUtil.oraclePrivKey.schnorrPublicKey
    val eventId = DLCWalletUtil.sampleOracleInfo.announcement.eventTLV match {
      case v0: OracleEventV0TLV => v0.eventId
    }

    (OracleAttestmentV0TLV(eventId,
                           publicKey,
                           Vector(initiatorWinSig),
                           Vector(initiatorWinStr)),
     OracleAttestmentV0TLV(eventId,
                           publicKey,
                           Vector(recipientWinSig),
                           Vector(recipientWinStr)))
  }

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

      val fundingTxVerify = fundingTx.inputs.zipWithIndex.forall {
        case (input, index) =>
          val output = fundingTxPrevOutputRefs
            .find(_.outPoint == input.previousOutput)
            .get
            .output
          verifyInput(fundingTx, index, output)
      }
      assert(fundingTxVerify)
    }
  }

  it must "do a unilateral close as the initiator" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      status <- getDLCStatus(wallets._1.wallet)
      (sig, _) = getSigs(status.contractInfo)
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)
      _ = println(s"result=$result")
      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      dlcId = status.dlcId
      _ = println(s"status=$status dlcId=$dlcId")
      statusAOpt <- wallets._1.wallet.findDLC(dlcId)
      _ = println(s"statusAOpt=$statusAOpt")
      statusBOpt <- wallets._2.wallet.findDLC(dlcId)
      _ = println(s"statusBOpt=${statusBOpt}")
      _ = {
        (statusAOpt, statusBOpt) match {
          case (Some(statusA: Claimed), Some(statusB: RemoteClaimed)) =>
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
      (_, sig) = getSigs(status.contractInfo)
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)

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

  it must "execute a losing dlc" in { wallets =>
    val dlcA = wallets._1.wallet

    for {
      contractId <- getContractId(wallets._1.wallet)
      status <- getDLCStatus(dlcA)
      // use dlcB winning sigs
      (_, sig) = getSigs(status.contractInfo)

      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)

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
      walletA.createDLCOffer(contractInfoTLV = contractInfo,
                             collateral = totalCollateral,
                             feeRateOpt = feeRateOpt,
                             locktime = UInt32.zero,
                             refundLT = UInt32.one)
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
}
