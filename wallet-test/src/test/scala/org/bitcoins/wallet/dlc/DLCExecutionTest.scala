package org.bitcoins.wallet.dlc

import org.bitcoins.core.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.testkit.wallet.DLCWalletUtil.InitializedDLCWallet
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.scalatest.FutureOutcome

class DLCExecutionTest extends BitcoinSDualWalletTest {
  type FixtureParam = (InitializedDLCWallet, InitializedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test)
  }

  behavior of "DLCExecution"

  val eventId: Sha256DigestBE = DLCWalletUtil.sampleDLCEventId
  val winSig: SchnorrDigitalSignature = DLCWalletUtil.sampleOracleWinSig
  val loseSig: SchnorrDigitalSignature = DLCWalletUtil.sampleOracleLoseSig

  it must "get the correct funding transaction" in {
    case (dlcWalletA: InitializedDLCWallet, dlcWalletB: InitializedDLCWallet) =>
      val dlcA = dlcWalletA.wallet
      val dlcB = dlcWalletB.wallet

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
      }
  }

  it must "do a dlc mutual close where the initiator wins" in {
    case (dlcWalletA: InitializedDLCWallet, dlcWalletB: InitializedDLCWallet) =>
      val dlcA = dlcWalletA.wallet
      val dlcB = dlcWalletB.wallet

      for {
        offerOpt <- dlcA.dlcOfferDAO.findByEventId(eventId)

        closeSig <- dlcA.initDLCMutualClose(eventId, winSig)
        tx <- dlcB.acceptDLCMutualClose(closeSig)
      } yield {
        assert(offerOpt.isDefined)
        val offer = offerOpt.get

        assert(tx.inputs.size == 1)
        assert(tx.outputs.size == 1)
        assert(tx.outputs.head.scriptPubKey == offer.finalAddress.scriptPubKey)
        assert(ScriptInterpreter.checkTransaction(tx))
      }
  }

  it must "do a dlc mutual close where the recipient wins" in {
    case (dlcWalletA: InitializedDLCWallet, dlcWalletB: InitializedDLCWallet) =>
      val dlcA = dlcWalletA.wallet
      val dlcB = dlcWalletB.wallet

      for {
        acceptOpt <- dlcB.dlcAcceptDAO.findByEventId(eventId)

        closeSig <- dlcB.initDLCMutualClose(eventId, loseSig)
        tx <- dlcA.acceptDLCMutualClose(closeSig)
      } yield {
        assert(acceptOpt.isDefined)
        val accept = acceptOpt.get

        assert(tx.inputs.size == 1)
        assert(tx.outputs.size == 1)
        assert(tx.outputs.head.scriptPubKey == accept.finalAddress.scriptPubKey)
        assert(ScriptInterpreter.checkTransaction(tx))
      }
  }

  it must "do a unilateral close as the initiator" in {
    case (dlcWalletA: InitializedDLCWallet, _: InitializedDLCWallet) =>
      val dlcA = dlcWalletA.wallet

      for {
        (cet, closeTxOpt) <- dlcA.executeDLCForceClose(eventId, winSig)

      } yield {
        assert(closeTxOpt.isDefined)
        val closeTx = closeTxOpt.get

        assert(cet.inputs.size == 1)
        assert(cet.outputs.size == 1)
        assert(ScriptInterpreter.checkTransaction(cet))
        assert(ScriptInterpreter.checkTransaction(closeTx))
      }
  }

  it must "do a unilateral close as the recipient" in {
    case (_: InitializedDLCWallet, dlcWalletB: InitializedDLCWallet) =>
      val dlcB = dlcWalletB.wallet

      for {
        (cet, closeTxOpt) <- dlcB.executeDLCForceClose(eventId, loseSig)

      } yield {
        assert(closeTxOpt.isDefined)
        val closeTx = closeTxOpt.get

        assert(cet.inputs.size == 1)
        assert(cet.outputs.size == 1)
        assert(ScriptInterpreter.checkTransaction(cet))
        assert(ScriptInterpreter.checkTransaction(closeTx))
      }
  }

  it must "do a refund on a dlc as the initiator" in {
    case (dlcWalletA: InitializedDLCWallet, _: InitializedDLCWallet) =>
      val dlcA = dlcWalletA.wallet

      for {
        (refundTx, closeTxOpt) <- dlcA.executeDLCRefund(eventId)
      } yield {
        assert(closeTxOpt.isDefined)
        val closeTx = closeTxOpt.get

        assert(refundTx.inputs.size == 1)
        assert(refundTx.outputs.size == 2)
        assert(ScriptInterpreter.checkTransaction(refundTx))
        assert(ScriptInterpreter.checkTransaction(closeTx))
      }
  }

  it must "do a refund on a dlc as the recipient" in {
    case (_: InitializedDLCWallet, dlcWalletB: InitializedDLCWallet) =>
      val dlcB = dlcWalletB.wallet

      for {
        (refundTx, closeTxOpt) <- dlcB.executeDLCRefund(eventId)
      } yield {
        assert(closeTxOpt.isDefined)
        val closeTx = closeTxOpt.get

        assert(refundTx.inputs.size == 1)
        assert(refundTx.outputs.size == 2)
        assert(ScriptInterpreter.checkTransaction(refundTx))
        assert(ScriptInterpreter.checkTransaction(closeTx))
      }
  }

  it must "do a penalty transaction on a dlc as the initiator" in {
    case (dlcWalletA: InitializedDLCWallet, dlcWalletB: InitializedDLCWallet) =>
      val dlcA = dlcWalletA.wallet
      val dlcB = dlcWalletB.wallet

      for {
        (forceCloseTx, _) <- dlcB.executeDLCForceClose(eventId, loseSig)
        penaltyTxOpt <- dlcA.claimDLCPenaltyFunds(eventId, forceCloseTx)
      } yield {
        assert(penaltyTxOpt.isDefined)
        val penaltyTx = penaltyTxOpt.get

        assert(penaltyTx.inputs.size == 1)
        assert(penaltyTx.outputs.size == 1)
        assert(ScriptInterpreter.checkTransaction(penaltyTx))
      }
  }

  it must "do a penalty transaction on a dlc as the recipient" in {
    case (dlcWalletA: InitializedDLCWallet, dlcWalletB: InitializedDLCWallet) =>
      val dlcA = dlcWalletA.wallet
      val dlcB = dlcWalletB.wallet

      for {
        (forceCloseTx, _) <- dlcA.executeDLCForceClose(eventId, winSig)
        penaltyTxOpt <- dlcB.claimDLCPenaltyFunds(eventId, forceCloseTx)
      } yield {
        assert(penaltyTxOpt.isDefined)
        val penaltyTx = penaltyTxOpt.get

        assert(penaltyTx.inputs.size == 1)
        assert(penaltyTx.outputs.size == 1)
        assert(ScriptInterpreter.checkTransaction(penaltyTx))
      }
  }
}
