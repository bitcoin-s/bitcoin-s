package org.bitcoins.core.wallet.builder

import org.bitcoins.core.crypto.{ECPrivateKey, TxSigComponent, WitnessTxSigComponentRaw}
import org.bitcoins.core.gen.{CreditingTxGen, TransactionGenerators}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.{ScriptOk, ScriptResult}
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

import scala.annotation.tailrec

class TxBuilderSpec extends Properties("TxBuilderSpec") {
  type CreditingTxInfo = (Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])
  private val logger = BitcoinSLogger.logger
  property("sign a mix of spks in a tx and then have it verified") = {
    Prop.forAllNoShrink(CreditingTxGen.outputs, TransactionGenerators.smallOutputs) {
      case (creditingTxsInfo,destinations) =>
        val outpointsWithKeys = buildCreditingTxInfo(creditingTxsInfo)
        val builder = TxBuilder(destinations, creditingTxsInfo.map(_._1),outpointsWithKeys)
        val result = builder.get.sign({_ => true})
        result match {
          case Left(tx) =>
            val noRedeem = creditingTxsInfo.map(c => (c._1, c._2, c._3))
            verifyScript(tx,noRedeem)
          case Right(err) =>
            logger.error("error with p2pkh txoutputgen: " + err)
            false
        }
    }
  }

/*
  property("sign a p2pk tx and then have it verified") = {
    Prop.forAllNoShrink(CreditingTxGen.p2wpkhOutput, TransactionGenerators.smallOutputs) {
      case (creditingTxsInfo,destinations) =>
        val outpointsKeysRedeemScripts = buildCreditingTxInfo(Seq(creditingTxsInfo))
        val builder = TxBuilder(destinations, Seq(creditingTxsInfo._1),outpointsKeysRedeemScripts)
        val result = builder.get.sign({_ => true})
        result match {
          case Left(tx) =>
            verifyScript(tx,Seq((creditingTxsInfo._1, creditingTxsInfo._2, creditingTxsInfo._3)))
          case Right(err) =>
            logger.info("error with p2pkh txoutputgen: " + err)
            false
        }
    }
  }
*/


  private def buildCreditingTxInfo(info: Seq[CreditingTxInfo]): Map[TransactionOutPoint, (Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])] = {
    @tailrec
    def loop(rem: Seq[CreditingTxInfo],
             accum: Map[TransactionOutPoint, (Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])]): Map[TransactionOutPoint, (Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])] = rem match {
      case Nil => accum
      case h :: t =>
        val o = TransactionOutPoint(h._1.txId,UInt32(h._2))
        val outPointsSpendingInfo = (h._3, h._4, h._5)
        loop(t,accum.updated(o,outPointsSpendingInfo))
    }
    loop(info,Map.empty)
  }

  def verifyScript(tx: Transaction, creditingTxsInfo: Seq[(Transaction, Int, Seq[ECPrivateKey])]): Boolean = {
    val results: Seq[ScriptResult] = tx.inputs.zipWithIndex.map { case (input: TransactionInput,idx: Int) =>
      logger.info(s"evaulating input at idx $idx")
      val outpoint = input.previousOutput
      val creditingTx = creditingTxsInfo.find(_._1.txId == outpoint.txId).get
      val output = creditingTx._1.outputs(creditingTx._2)
      val spk =  output.scriptPubKey
      val amount = output.value
      val txSigComponent = spk match {
        case witSPK: WitnessScriptPubKeyV0 => WitnessTxSigComponentRaw(tx.asInstanceOf[WitnessTransaction],UInt32(idx),
          witSPK, Policy.standardFlags, amount)
        case _: UnassignedWitnessScriptPubKey => ???
        case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: WitnessCommitment
          | _: CSVScriptPubKey | _: CLTVScriptPubKey | _: NonStandardScriptPubKey | _: EscrowTimeoutScriptPubKey
          | EmptyScriptPubKey) =>
          TxSigComponent(tx,UInt32(idx),x,Policy.standardFlags)
        case p2sh: P2SHScriptPubKey =>
          //TODO: This is probably going to need to be changed with P2WSH
          TxSigComponent(tx,UInt32(idx),p2sh,Policy.standardFlags)
      }
      val program = ScriptProgram(txSigComponent)
      val result = ScriptInterpreter.run(program)
      logger.info(s"evaulating input at idx $idx result: $result")
      result
    }
    !results.exists(_ != ScriptOk)
  }
}
