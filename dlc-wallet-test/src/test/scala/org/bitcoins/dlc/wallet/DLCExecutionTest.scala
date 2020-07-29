package org.bitcoins.dlc.wallet

import org.bitcoins.core.currency.Satoshis
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
  OracleAttestmentTLV,
  OracleAttestmentV0TLV,
  OracleEventV0TLV
}
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.scalatest.FutureOutcome

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
      offerDb <- getInitialOffer(dlcA)
      paramHash = offerDb.paramHash
      offerOpt <- dlcA.dlcOfferDAO.findByParamHash(paramHash)
      acceptOpt <- dlcB.dlcAcceptDAO.findByParamHash(paramHash)

      inputsA <- dlcA.dlcInputsDAO.findByParamHash(paramHash)
      inputsB <- dlcB.dlcInputsDAO.findByParamHash(paramHash)

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
      offer <- getInitialOffer(wallets._1.wallet)
      (sig, _) = getSigs(offer.contractInfo)
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      paramHash = dlcDbAOpt.get.paramHash

      statusAOpt <- wallets._1.wallet.findDLC(paramHash)
      statusBOpt <- wallets._2.wallet.findDLC(paramHash)

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
      offer <- getInitialOffer(wallets._2.wallet)
      (_, sig) = getSigs(offer.contractInfo)
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = false,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      paramHash = dlcDbAOpt.get.paramHash

      statusAOpt <- wallets._1.wallet.findDLC(paramHash)
      statusBOpt <- wallets._2.wallet.findDLC(paramHash)

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
      offer <- getInitialOffer(dlcA)
      // use dlcB winning sigs
      (_, sig) = getSigs(offer.contractInfo)

      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      paramHash = dlcDbAOpt.get.paramHash

      statusAOpt <- wallets._1.wallet.findDLC(paramHash)
      statusBOpt <- wallets._2.wallet.findDLC(paramHash)

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
      func = (wallet: DLCWallet) => wallet.executeDLCRefund(contractId)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 2)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      paramHash = dlcDbAOpt.get.paramHash

      statusAOpt <- wallets._1.wallet.findDLC(paramHash)
      statusBOpt <- wallets._2.wallet.findDLC(paramHash)

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
      func = (wallet: DLCWallet) => wallet.executeDLCRefund(contractId)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = false,
                                 func = func,
                                 expectedOutputs = 2)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      paramHash = dlcDbAOpt.get.paramHash

      statusAOpt <- wallets._1.wallet.findDLC(paramHash)
      statusBOpt <- wallets._2.wallet.findDLC(paramHash)

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
}
