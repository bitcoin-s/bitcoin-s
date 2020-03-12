package org.bitcoins.wallet.dlc

import org.bitcoins.core.crypto.{
  SchnorrDigitalSignature,
  Sha256DigestBE,
  WitnessTxSigComponent
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.testkit.wallet.DLCWalletUtil.InitializedDLCWallet
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.bitcoins.wallet.Wallet
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future

class DLCExecutionTest extends BitcoinSDualWalletTest {
  type FixtureParam = (InitializedDLCWallet, InitializedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test)
  }

  behavior of "DLCWallet"

  val eventId: Sha256DigestBE = DLCWalletUtil.sampleDLCEventId
  val winSig: SchnorrDigitalSignature = DLCWalletUtil.sampleOracleWinSig
  val loseSig: SchnorrDigitalSignature = DLCWalletUtil.sampleOracleLoseSig

  val flags: Seq[ScriptFlag] = Policy.standardFlags

  def mutualCloseTest(
      wallets: FixtureParam,
      asInitiator: Boolean): Future[Assertion] = {
    val (dlcA, dlcB, sig) =
      if (asInitiator) (wallets._1.wallet, wallets._2.wallet, winSig)
      else (wallets._2.wallet, wallets._1.wallet, loseSig)

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
        assert(tx.outputs.head.scriptPubKey == offer.finalAddress.scriptPubKey)
      } else {
        assert(tx.outputs.head.scriptPubKey == accept.finalAddress.scriptPubKey)
      }
      assert(ScriptInterpreter.checkTransaction(tx))

      val sigComponent =
        WitnessTxSigComponent(tx.asInstanceOf[WitnessTransaction],
                              UInt32.zero,
                              fundingTx.outputs.head,
                              flags)
      assert(
        ScriptInterpreter.runVerify(PreExecutionScriptProgram(sigComponent)))
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
      fundingTx <- dlcB.getDLCFundingTx(eventId)
      (tx, closeTxOpt) <- if (asInitiator) func(dlcA) else func(dlcB)
    } yield {
      assert(closeTxOpt.isDefined)
      val closeTx = closeTxOpt.get

      assert(tx.inputs.size == 1)
      assert(tx.outputs.size == expectedOutputs)
      assert(ScriptInterpreter.checkTransaction(tx))
      assert(ScriptInterpreter.checkTransaction(closeTx))

      val txSigComponent =
        WitnessTxSigComponent(tx.asInstanceOf[WitnessTransaction],
                              UInt32.zero,
                              fundingTx.outputs.head,
                              flags)
      assert(
        ScriptInterpreter.runVerify(PreExecutionScriptProgram(txSigComponent)))

      val output = if (asInitiator) tx.outputs.head else tx.outputs.last

      val closeSigComponent =
        WitnessTxSigComponent(closeTx.asInstanceOf[WitnessTransaction],
                              UInt32.zero,
                              output,
                              flags)
      assert(
        ScriptInterpreter.runVerify(
          PreExecutionScriptProgram(closeSigComponent)))
    }
  }

  it must "get the correct funding transaction" in { wallets =>
    val dlcA = wallets._1.wallet
    val dlcB = wallets._2.wallet

    for {
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
          val sigComponent =
            WitnessTxSigComponent(fundingTx.asInstanceOf[WitnessTransaction],
                                  UInt32(index),
                                  output,
                                  flags)
          ScriptInterpreter.runVerify(PreExecutionScriptProgram(sigComponent))
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

  it must "do a unilateral close as the initiator" in { wallets =>
    val func = (wallet: Wallet) => wallet.executeDLCForceClose(eventId, winSig)

    dlcExecutionTest(wallets = wallets,
                     asInitiator = true,
                     func = func,
                     expectedOutputs = 1)
  }

  it must "do a unilateral close as the recipient" in { wallets =>
    val func = (wallet: Wallet) => wallet.executeDLCForceClose(eventId, loseSig)

    dlcExecutionTest(wallets = wallets,
                     asInitiator = false,
                     func = func,
                     expectedOutputs = 1)
  }

  it must "do a refund on a dlc as the initiator" in { wallets =>
    val func = (wallet: Wallet) => wallet.executeDLCRefund(eventId)

    dlcExecutionTest(wallets = wallets,
                     asInitiator = true,
                     func = func,
                     expectedOutputs = 2)
  }

  it must "do a refund on a dlc as the recipient" in { wallets =>
    val func = (wallet: Wallet) => wallet.executeDLCRefund(eventId)

    dlcExecutionTest(wallets = wallets,
                     asInitiator = false,
                     func = func,
                     expectedOutputs = 2)
  }

  it must "do a penalty transaction on a dlc as the initiator" in { wallets =>
    val dlcA = wallets._1.wallet
    val dlcB = wallets._2.wallet

    for {
      (forceCloseTx, _) <- dlcB.executeDLCForceClose(eventId, loseSig)
      penaltyTxOpt <- dlcA.claimDLCPenaltyFunds(eventId, forceCloseTx)
    } yield {
      assert(penaltyTxOpt.isDefined)
      val penaltyTx = penaltyTxOpt.get

      assert(penaltyTx.inputs.size == 1)
      assert(penaltyTx.outputs.size == 1)
      assert(ScriptInterpreter.checkTransaction(penaltyTx))

      val sigComponent =
        WitnessTxSigComponent(penaltyTx.asInstanceOf[WitnessTransaction],
                              UInt32.zero,
                              forceCloseTx.outputs.head,
                              flags)
      assert(
        ScriptInterpreter.runVerify(PreExecutionScriptProgram(sigComponent)))
    }
  }

  it must "do a penalty transaction on a dlc as the recipient" in { wallets =>
    val dlcA = wallets._1.wallet
    val dlcB = wallets._2.wallet

    for {
      (forceCloseTx, _) <- dlcA.executeDLCForceClose(eventId, winSig)
      penaltyTxOpt <- dlcB.claimDLCPenaltyFunds(eventId, forceCloseTx)
    } yield {
      assert(penaltyTxOpt.isDefined)
      val penaltyTx = penaltyTxOpt.get

      assert(penaltyTx.inputs.size == 1)
      assert(penaltyTx.outputs.size == 1)
      assert(ScriptInterpreter.checkTransaction(penaltyTx))

      val sigComponent =
        WitnessTxSigComponent(penaltyTx.asInstanceOf[WitnessTransaction],
                              UInt32.zero,
                              forceCloseTx.outputs.head,
                              flags)
      assert(
        ScriptInterpreter.runVerify(PreExecutionScriptProgram(sigComponent)))
    }
  }
}
