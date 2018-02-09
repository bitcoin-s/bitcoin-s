package org.bitcoins.core.wallet.builder

import org.bitcoins.core.crypto.{ECPrivateKey, TxSigComponent}
import org.bitcoins.core.gen.{CreditingTxGen, TransactionGenerators}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput, TransactionOutPoint}
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.{ScriptOk, ScriptResult}
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

import scala.annotation.tailrec

class TxBuilderSpec extends Properties("TxBuilderSpec") {
  private val logger = BitcoinSLogger.logger
  property("sign a mix of spks in a tx and then have it verified") = {
    Prop.forAllNoShrink(CreditingTxGen.outputs, TransactionGenerators.smallOutputs) {
      case (creditingTxsInfo,destinations) =>
        val outpointsWithKeys = buildOutpointsKeysRedeemScript(creditingTxsInfo)
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
    Prop.forAllNoShrink(CreditingTxGen.p2shOutputs, TransactionGenerators.smallOutputs) {
      case (creditingTxsInfo,destinations) =>
        val outpointsKeysRedeemScripts = buildOutpointsKeysRedeemScript(creditingTxsInfo)
        val builder = TxBuilder(destinations, creditingTxsInfo.map(_._1),outpointsKeysRedeemScripts)
        val result = builder.get.sign({_ => true})
        result match {
          case Left(tx) =>
            verifyScript(tx,creditingTxsInfo.map(c => (c._1, c._2, c._3)))
          case Right(err) =>
            logger.info("error with p2pkh txoutputgen: " + err)
            false
        }
    }
  }*/


  private def buildOutpointsKeysRedeemScript(info: Seq[(Transaction, Int,
    Seq[ECPrivateKey], Option[ScriptPubKey])]): Map[TransactionOutPoint, (Seq[ECPrivateKey], Option[ScriptPubKey])] = {
    @tailrec
    def loop(rem: Seq[(Transaction,Int, Seq[ECPrivateKey], Option[ScriptPubKey])],
             accum: Map[TransactionOutPoint, (Seq[ECPrivateKey], Option[ScriptPubKey])]): Map[TransactionOutPoint, (Seq[ECPrivateKey], Option[ScriptPubKey])] = rem match {
      case Nil => accum
      case h :: t =>
        val o = TransactionOutPoint(h._1.txId,UInt32(h._2))
        val keysWithRedeemScript = (h._3, h._4)
        loop(t,accum.updated(o,keysWithRedeemScript))
    }
    loop(info,Map.empty)
  }

  def verifyScript(tx: Transaction, creditingTxsInfo: Seq[(Transaction, Int, Seq[ECPrivateKey])]): Boolean = {
    val results: Seq[ScriptResult] = tx.inputs.zipWithIndex.map { case (input: TransactionInput,idx: Int) =>
      logger.info(s"evaulating input at idx $idx")
      val outpoint = input.previousOutput
      val creditingTx = creditingTxsInfo.find(_._1.txId == outpoint.txId).get
      val spk = creditingTx._1.outputs(creditingTx._2).scriptPubKey
      val txSigComponent = TxSigComponent(tx,UInt32(idx),spk,Policy.standardFlags)
      val program = ScriptProgram(txSigComponent)
      val result = ScriptInterpreter.run(program)
      logger.info(s"evaulating input at idx $idx result: $result")
      result
    }
    !results.exists(_ != ScriptOk)
  }
}
