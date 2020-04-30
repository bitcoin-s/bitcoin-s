package org.bitcoins.wallet.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.ContractInfo
import org.bitcoins.core.crypto.WitnessTxSigComponent
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutput,
  WitnessTransaction
}
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
import org.bitcoins.testkit.wallet.DLCWalletUtil.InitializedDLCWallet
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.models.DLCOfferDb
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future
import scala.math.Ordering

class DLCExecutionTest extends BitcoinSDualWalletTest {
  type FixtureParam = (InitializedDLCWallet, InitializedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test)
  }

  behavior of "DLCWallet"

  def getInitialOffer(wallet: Wallet): Future[DLCOfferDb] = {
    wallet.dlcOfferDAO.findAll().map { all =>
      require(all.size == 1, "There should only be one dlc initialized")
      all.head
    }
  }

  def getSigs(contractInfo: ContractInfo): (
      SchnorrDigitalSignature,
      SchnorrDigitalSignature) = {

    // Get a hash that the initiator wins for
    val initiatorWinHash =
      contractInfo
        .max(new Ordering[(Sha256DigestBE, Satoshis)] {
          override def compare(
              x: (Sha256DigestBE, Satoshis),
              y: (Sha256DigestBE, Satoshis)): Int =
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

  def verifyInput(
      transaction: Transaction,
      inputIndex: Long,
      prevOut: TransactionOutput): Boolean = {
    val sigComponent = WitnessTxSigComponent(
      transaction.asInstanceOf[WitnessTransaction],
      UInt32(inputIndex),
      prevOut,
      Policy.standardFlags
    )
    ScriptInterpreter.runVerify(PreExecutionScriptProgram(sigComponent))
  }

  def mutualCloseTest(
      wallets: FixtureParam,
      asInitiator: Boolean): Future[Assertion] = {
    getInitialOffer(wallets._1.wallet).flatMap { offer =>
      val eventId = offer.eventId

      val (initiatorWinSig, recipientWinSig) = getSigs(offer.contractInfo)

      val (dlcA, dlcB, sig) =
        if (asInitiator) (wallets._1.wallet, wallets._2.wallet, initiatorWinSig)
        else (wallets._2.wallet, wallets._1.wallet, recipientWinSig)

      for {
        offerOpt <- dlcA.dlcOfferDAO.findByEventId(eventId)
        acceptOpt <- dlcA.dlcAcceptDAO.findByEventId(eventId)

        fundingTx <- dlcA.getDLCFundingTx(eventId)

        closeSig <- dlcA.initDLCMutualClose(eventId, sig)
        tx <- dlcB.acceptDLCMutualClose(closeSig)
      } yield {
        assert(offerOpt.isDefined)
        val offer = offerOpt.get
        assert(acceptOpt.isDefined)
        val accept = acceptOpt.get

        assert(tx.inputs.size == 1)
        assert(tx.outputs.size == 1)
        if (asInitiator) {
          assert(
            tx.outputs.head.scriptPubKey == offer.finalAddress.scriptPubKey)
        } else {
          assert(
            tx.outputs.head.scriptPubKey == accept.finalAddress.scriptPubKey)
        }
        assert(ScriptInterpreter.checkTransaction(tx))

        assert(verifyInput(tx, 0, fundingTx.outputs.head))
      }
    }
  }

  def dlcExecutionTest(
      wallets: FixtureParam,
      asInitiator: Boolean,
      func: Wallet => Future[(Transaction, Option[Transaction])],
      expectedOutputs: Int): Future[Assertion] = {
    val dlcA = wallets._1.wallet
    val dlcB = wallets._2.wallet

    for {
      offer <- getInitialOffer(dlcA)
      fundingTx <- dlcB.getDLCFundingTx(offer.eventId)
      (tx, closeTxOpt) <- if (asInitiator) func(dlcA) else func(dlcB)
    } yield {
      assert(closeTxOpt.isDefined)
      val closeTx = closeTxOpt.get

      assert(tx.inputs.size == 1)
      assert(tx.outputs.size == expectedOutputs)
      assert(ScriptInterpreter.checkTransaction(tx))
      assert(ScriptInterpreter.checkTransaction(closeTx))

      assert(verifyInput(tx, 0, fundingTx.outputs.head))

      val output = if (asInitiator) tx.outputs.head else tx.outputs.last
      assert(verifyInput(closeTx, 0, output))
    }
  }

  it must "get the correct funding transaction" in { wallets =>
    val dlcA = wallets._1.wallet
    val dlcB = wallets._2.wallet

    for {
      offerDb <- getInitialOffer(dlcA)
      eventId = offerDb.eventId
      offerOpt <- dlcA.dlcOfferDAO.findByEventId(eventId)
      acceptOpt <- dlcB.dlcAcceptDAO.findByEventId(eventId)

      inputsA <- dlcA.dlcInputsDAO.findByEventId(eventId)
      inputsB <- dlcB.dlcInputsDAO.findByEventId(eventId)

      fundingTx <- dlcB.getDLCFundingTx(eventId)
    } yield {
      assert(offerOpt.isDefined)
      assert(acceptOpt.isDefined)

      val offer = offerOpt.get
      val accept = acceptOpt.get

      val comparableInputsA = inputsA.sortBy(_.outPoint.hex)
      val comparableInputsB =
        inputsB
          .sortBy(_.outPoint.hex)
          .map(_.copy(sigs = Vector.empty)) // initiator will not have funding sigs

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

  it must "do a dlc mutual close where the initiator wins" in { wallets =>
    mutualCloseTest(wallets = wallets, asInitiator = true)
  }

  it must "do a dlc mutual close where the recipient wins" in { wallets =>
    mutualCloseTest(wallets = wallets, asInitiator = false)
  }

  it must "fail to init a losing mutual close" in { wallets =>
    val dlcA = wallets._1.wallet

    val initMutualCloseF = for {
      offer <- getInitialOffer(dlcA)
      (_, sig) = getSigs(offer.contractInfo)

      closeSig <- dlcA.initDLCMutualClose(offer.eventId, sig)
    } yield closeSig

    recoverToSucceededIf[UnsupportedOperationException](initMutualCloseF)
  }

  it must "do a unilateral close as the initiator" in { wallets =>
    for {
      offer <- getInitialOffer(wallets._1.wallet)
      (sig, _) = getSigs(offer.contractInfo)
      func = (wallet: Wallet) => wallet.executeDLCForceClose(offer.eventId, sig)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)
    } yield result
  }

  it must "do a unilateral close as the recipient" in { wallets =>
    for {
      offer <- getInitialOffer(wallets._2.wallet)
      (_, sig) = getSigs(offer.contractInfo)
      func = (wallet: Wallet) => wallet.executeDLCForceClose(offer.eventId, sig)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = false,
                                 func = func,
                                 expectedOutputs = 1)
    } yield result
  }

  it must "fail to do losing unilateral close" in { wallets =>
    val dlcA = wallets._1.wallet

    val executeDLCForceCloseF = for {
      offer <- getInitialOffer(dlcA)
      (_, sig) = getSigs(offer.contractInfo)

      tx <- dlcA.executeDLCForceClose(offer.eventId, sig)
    } yield tx

    recoverToSucceededIf[UnsupportedOperationException](executeDLCForceCloseF)
  }

  it must "do a refund on a dlc as the initiator" in { wallets =>
    for {
      offer <- getInitialOffer(wallets._1.wallet)
      func = (wallet: Wallet) => wallet.executeDLCRefund(offer.eventId)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 2)
    } yield result
  }

  it must "do a refund on a dlc as the recipient" in { wallets =>
    for {
      offer <- getInitialOffer(wallets._2.wallet)
      func = (wallet: Wallet) => wallet.executeDLCRefund(offer.eventId)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = false,
                                 func = func,
                                 expectedOutputs = 2)
    } yield result
  }

  it must "do a penalty transaction on a dlc as the initiator" in { wallets =>
    val dlcA = wallets._1.wallet
    val dlcB = wallets._2.wallet

    for {
      offer <- getInitialOffer(dlcA)
      (_, sig) = getSigs(offer.contractInfo)
      (forceCloseTx, _) <- dlcB.executeDLCForceClose(offer.eventId, sig)
      penaltyTxOpt <- dlcA.claimDLCPenaltyFunds(offer.eventId, forceCloseTx)
    } yield {
      assert(penaltyTxOpt.isDefined)
      val penaltyTx = penaltyTxOpt.get

      assert(penaltyTx.inputs.size == 1)
      assert(penaltyTx.outputs.size == 1)
      assert(ScriptInterpreter.checkTransaction(penaltyTx))

      assert(verifyInput(penaltyTx, 0, forceCloseTx.outputs.head))
    }
  }

  it must "do a penalty transaction on a dlc as the recipient" in { wallets =>
    val dlcA = wallets._1.wallet
    val dlcB = wallets._2.wallet

    for {
      offer <- getInitialOffer(dlcA)
      (sig, _) = getSigs(offer.contractInfo)
      (forceCloseTx, _) <- dlcA.executeDLCForceClose(offer.eventId, sig)
      penaltyTxOpt <- dlcB.claimDLCPenaltyFunds(offer.eventId, forceCloseTx)
    } yield {
      assert(penaltyTxOpt.isDefined)
      val penaltyTx = penaltyTxOpt.get

      assert(penaltyTx.inputs.size == 1)
      assert(penaltyTx.outputs.size == 1)
      assert(ScriptInterpreter.checkTransaction(penaltyTx))

      assert(verifyInput(penaltyTx, 0, forceCloseTx.outputs.head))
    }
  }
}
