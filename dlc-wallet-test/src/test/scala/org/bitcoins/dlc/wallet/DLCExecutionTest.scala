package org.bitcoins.dlc.wallet

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.ContractInfo
import org.bitcoins.commons.jsonmodels.dlc.DLCState
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256Digest}
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.scalatest.FutureOutcome

import scala.math.Ordering

class DLCExecutionTest extends BitcoinSDualWalletTest {
  type FixtureParam = (InitializedDLCWallet, InitializedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test)
  }

  behavior of "DLCWallet"

  def getSigs(contractInfo: ContractInfo): (
      SchnorrDigitalSignature,
      SchnorrDigitalSignature) = {

    // Get a hash that the initiator wins for
    val initiatorWinHash =
      contractInfo
        .max(new Ordering[(Sha256Digest, Satoshis)] {
          override def compare(
              x: (Sha256Digest, Satoshis),
              y: (Sha256Digest, Satoshis)): Int =
            x._2.compare(y._2)
        })
        ._1
    val initiatorWinSig = DLCWalletUtil.oraclePrivKey
      .schnorrSignWithNonce(initiatorWinHash.bytes, DLCWalletUtil.kValue)

    // Get a hash that the recipient wins for
    val recipientWinHash =
      contractInfo.find(_._2 == Satoshis.zero).get._1
    val recipientWinSig = DLCWalletUtil.oraclePrivKey
      .schnorrSignWithNonce(recipientWinHash.bytes, DLCWalletUtil.kValue)

    (initiatorWinSig, recipientWinSig)
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

    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.RemoteClaimed)
          assert(dlcB.state == DLCState.Claimed)
        case (_, _) => fail()
      }
    }
  }

  it must "fail to do losing unilateral close" in { wallets =>
    val dlcA = wallets._1.wallet

    val executeDLCForceCloseF = for {
      contractId <- getContractId(wallets._1.wallet)
      offer <- getInitialOffer(dlcA)
      (_, sig) = getSigs(offer.contractInfo)

      tx <- dlcA.executeDLC(contractId, sig)
    } yield tx

    recoverToSucceededIf[UnsupportedOperationException](executeDLCForceCloseF)
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

    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.Refunded)
          assert(dlcB.state == DLCState.Refunded)
        case (_, _) => fail()
      }
    }
  }
}
