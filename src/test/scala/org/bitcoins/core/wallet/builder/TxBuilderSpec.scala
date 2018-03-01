package org.bitcoins.core.wallet.builder

import org.bitcoins.core.crypto.{ECPrivateKey, TxSigComponent, WitnessTxSigComponentRaw}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.gen.{CreditingTxGen, TransactionGenerators}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.{ScriptErrorPushSize, ScriptOk, ScriptResult}
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

import scala.annotation.tailrec

class TxBuilderSpec extends Properties("TxBuilderSpec") {
  type CreditingTxInfo = (Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness], HashType)
  type OutPointMap = Map[TransactionOutPoint, (TransactionOutput, Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness], HashType)]

  private val logger = BitcoinSLogger.logger
  private val tc = TransactionConstants
  property("sign a mix of spks in a tx and then have it verified") = {
    Prop.forAllNoShrink(CreditingTxGen.outputs) {
      case creditingTxsInfo =>
        val creditingOutputs = creditingTxsInfo.map(c => c._1.outputs(c._2).value)
        val totalAmount = creditingOutputs.fold(CurrencyUnits.zero)(_ + _)
        Prop.forAll(TransactionGenerators.smallOutputs(totalAmount)) { destinations: Seq[TransactionOutput] =>
          val fee = 1000 //sat/vbyte
          val outpointsWithKeys = buildCreditingTxInfo(creditingTxsInfo)
          val builder = TxBuilder(destinations, creditingTxsInfo.map(_._1),outpointsWithKeys,fee,EmptyScriptPubKey)
          val result = builder.left.flatMap(_.sign({(_,_) => true}))
          result match {
            case Left(tx) =>
              val noRedeem = creditingTxsInfo.map(c => (c._1, c._2, c._3))
              verifyScript(tx,noRedeem)
            case Right(err) =>
              logger.error("error with p2pkh txoutputgen: " + err)
              err == ScriptErrorPushSize
          }
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


  private def buildCreditingTxInfo(info: Seq[CreditingTxInfo]): OutPointMap = {
    @tailrec
    def loop(rem: Seq[CreditingTxInfo],
             accum: OutPointMap): OutPointMap = rem match {
      case Nil => accum
      case h :: t =>
        val tx = h._1
        val o = TransactionOutPoint(tx.txId,UInt32(h._2))
        val output = h._1.outputs(h._2)
        val outPointsSpendingInfo = (output, h._3, h._4, h._5, h._6)
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
