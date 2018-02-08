package org.bitcoins.core.wallet.builder

import org.bitcoins.core.crypto.{ECPrivateKey, TxSigComponent}
import org.bitcoins.core.gen.{CreditingTxGen, TransactionGenerators}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput, TransactionOutPoint}
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.{ScriptOk, ScriptResult}
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

import scala.annotation.tailrec

class TxBuilderSpec extends Properties("TxBuilderSpec") {
  private val logger = BitcoinSLogger.logger
  property("sign a p2pkh tx and then have it verified") = {
    Prop.forAllNoShrink(CreditingTxGen.p2pkhOutputs, TransactionGenerators.outputs) {
      case (creditingTxsInfo,destinations) =>
        val outpointsWithKeys = buildOutpointsWithKeys(creditingTxsInfo)
        val builder = TxBuilder(destinations, creditingTxsInfo.map(_._1),outpointsWithKeys)
        val result = builder.get.sign({_ => true})
        result match {
          case Left(tx) =>
            verifyScript(tx,creditingTxsInfo)
          case Right(err) =>
            logger.info("error with p2pkh txoutputgen: " + err)
            false
        }
    }
  }

/*  property("sign a p2pk tx and then have it verified") = {
    Prop.forAllNoShrink(CreditingTxGen.p2pkOutputs, TransactionGenerators.outputs) {
      case (creditingTxsInfo,destinations) =>
        val outpointsWithKeys = buildOutpointsWithKeys(creditingTxsInfo)
        val builder = TxBuilder(destinations, creditingTxsInfo.map(_._1),outpointsWithKeys)
        val result = builder.get.sign({_ => true})
        result match {
          case Left(tx) =>
            verifyScript(tx,creditingTxsInfo)
          case Right(err) =>
            logger.info("error with p2pkh txoutputgen: " + err)
            false
        }
    }
  }*/


  private def buildOutpointsWithKeys(info: Seq[(Transaction, Int, ECPrivateKey)]): Map[TransactionOutPoint, Seq[ECPrivateKey]] = {
    @tailrec
    def loop(rem: Seq[(Transaction,Int,ECPrivateKey)],
             accum: Map[TransactionOutPoint, Seq[ECPrivateKey]]): Map[TransactionOutPoint, Seq[ECPrivateKey]] = rem match {
      case Nil => accum
      case h :: t =>
        val o = TransactionOutPoint(h._1.txId,UInt32(h._2))
        val keys = Seq(h._3)
        loop(t,accum.updated(o,keys))
    }
    loop(info,Map.empty)
  }

  def verifyScript(tx: Transaction, creditingTxsInfo: Seq[(Transaction, Int, ECPrivateKey)]): Boolean = {
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
