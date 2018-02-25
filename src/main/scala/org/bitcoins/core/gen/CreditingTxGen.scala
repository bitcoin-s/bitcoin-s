package org.bitcoins.core.gen

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.scalacheck.Gen

sealed abstract class CreditingTxGen {
  type CreditingTxInfo = (Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])
  /** Minimum amount of outputs to generate */
  private val min = 1
  /** Maximum amount of outputs to generate */
  private val max = 3

  private def nonEmptyOutputs: Gen[Seq[TransactionOutput]] = Gen.choose(1,5).flatMap { n =>
    Gen.listOfN(n, TransactionGenerators.output)
  }

  def rawOutput: Gen[CreditingTxInfo] = {
    Gen.oneOf(p2pkOutput, p2pkhOutput, multiSigOutput, cltvOutput, csvOutput)
  }

  def basicOutput: Gen[CreditingTxInfo] = {
    Gen.oneOf(p2pkOutput,
      p2pkhOutput, multiSigOutput)
  }

  def nonP2WSHOutput: Gen[CreditingTxInfo] = rawOutput

  def output: Gen[CreditingTxInfo] = Gen.oneOf(p2pkOutput,
    p2pkhOutput, multiSigOutput, p2shOutput,
    csvOutput, cltvOutput,
    p2wpkhOutput, p2wshOutput)

  def outputs: Gen[Seq[CreditingTxInfo]] = {
    Gen.choose(min,5).flatMap(n => Gen.listOfN(n,output))
  }

  /** Generates a crediting tx with a p2pk spk at the returned index */
  def p2pkOutput: Gen[CreditingTxInfo] = ScriptGenerators.p2pkScriptPubKey.flatMap { p2pk =>
    build(p2pk._1,Seq(p2pk._2), None, None)
  }
  /** Generates multiple crediting txs with p2pk spks at the returned index */
  def p2pkOutputs: Gen[Seq[CreditingTxInfo]] = {
    Gen.choose(min,max).flatMap(n => Gen.listOfN(n,p2pkOutput))
  }

  /** Generates a transaction that has a p2pkh output at the returned index. This
    * output can be spent by the returned ECPrivateKey */
  def p2pkhOutput: Gen[CreditingTxInfo] = ScriptGenerators.p2pkhScriptPubKey.flatMap { p2pkh =>
    build(p2pkh._1,Seq(p2pkh._2), None, None)
  }

  /** Generates a sequence of p2pkh outputs at the returned index */
  def p2pkhOutputs: Gen[Seq[CreditingTxInfo]] = {
    Gen.choose(min,max).flatMap(n => Gen.listOfN(n,p2pkhOutput))
  }

  def multiSigOutput: Gen[CreditingTxInfo] = ScriptGenerators.multiSigScriptPubKey.flatMap { multisig =>
    build(multisig._1, multisig._2, None, None)
  }

  def multiSigOutputs: Gen[Seq[CreditingTxInfo]] = {
    Gen.choose(min,max).flatMap(n => Gen.listOfN(n,multiSigOutput))
  }

  def p2shOutput: Gen[CreditingTxInfo] = rawOutput.map { o =>
    val oldTx = o._1
    val oldOutput = oldTx.outputs(o._2)
    val redeemScript = oldTx.outputs(o._2).scriptPubKey
    val p2sh = P2SHScriptPubKey(redeemScript)
    val updatedOutput = TransactionOutput(oldOutput.value,p2sh)
    val tx = oldTx match {
      case btx: BaseTransaction => BaseTransaction(btx.version, btx.inputs,
        btx.outputs.updated(o._2,updatedOutput), btx.lockTime)
      case wtx: WitnessTransaction => WitnessTransaction(wtx.version, wtx.inputs,
        wtx.outputs.updated(o._2, updatedOutput), wtx.lockTime, wtx.witness)
    }
    (tx,o._2,o._3,Some(redeemScript), o._5)
  }

  def p2shOutputs: Gen[Seq[CreditingTxInfo]] = {
    Gen.choose(min,max).flatMap(n => Gen.listOfN(n,p2shOutput))
  }

  def cltvOutput: Gen[CreditingTxInfo] = TransactionGenerators.spendableCLTVValues.flatMap { case (scriptNum,_) =>
    basicOutput.map { o =>
      val oldTx = o._1
      val oldOutput = oldTx.outputs(o._2)
      val csvSPK = CLTVScriptPubKey(scriptNum,oldOutput.scriptPubKey)
      val updatedOutput = TransactionOutput(oldOutput.value,csvSPK)
      val tx = oldTx match {
        case btx: BaseTransaction => BaseTransaction(btx.version, btx.inputs,
          btx.outputs.updated(o._2,updatedOutput), btx.lockTime)
        case wtx: WitnessTransaction => WitnessTransaction(wtx.version, wtx.inputs,
          wtx.outputs.updated(o._2, updatedOutput), wtx.lockTime, wtx.witness)
      }
      (tx,o._2,o._3,o._4, o._5)
    }
  }

  def cltvOutputs: Gen[Seq[CreditingTxInfo]] = Gen.choose(min,max).flatMap(n => Gen.listOfN(n,cltvOutput))

  def csvOutput: Gen[CreditingTxInfo] = TransactionGenerators.spendableCSVValues.flatMap { case (scriptNum, _) =>
    basicOutput.map { o =>
      val oldTx = o._1
      val oldOutput = oldTx.outputs(o._2)
      val csvSPK = CSVScriptPubKey(scriptNum,oldOutput.scriptPubKey)
      val updatedOutput = TransactionOutput(oldOutput.value,csvSPK)
      val tx = oldTx match {
        case btx: BaseTransaction => BaseTransaction(btx.version, btx.inputs,
          btx.outputs.updated(o._2,updatedOutput), btx.lockTime)
        case wtx: WitnessTransaction => WitnessTransaction(wtx.version, wtx.inputs,
          wtx.outputs.updated(o._2, updatedOutput), wtx.lockTime, wtx.witness)
      }
      (tx,o._2,o._3,o._4, o._5)
    }
  }

  def csvOutputs: Gen[Seq[CreditingTxInfo]] = Gen.choose(min,max).flatMap(n => Gen.listOfN(n,csvOutput))

  def p2wpkhOutput: Gen[CreditingTxInfo] = ScriptGenerators.p2wpkhSPKV0.flatMap { witSPK =>
    val scriptWit = P2WPKHWitnessV0(witSPK._2.head.publicKey)
    build(witSPK._1,witSPK._2,None,Some(scriptWit))
  }

  def p2wpkhOutputs: Gen[Seq[CreditingTxInfo]] = Gen.choose(min,max).flatMap(n => Gen.listOfN(n,p2wpkhOutput))

  def p2wshOutput: Gen[CreditingTxInfo] = nonP2WSHOutput.flatMap { case (tx,outputIndex,privKeys,redeemScriptOpt,scriptWitOpt) =>
    val spk = tx.outputs(outputIndex).scriptPubKey
    val scriptWit = P2WSHWitnessV0(spk)
    val witSPK = P2WSHWitnessSPKV0(spk)
    build(witSPK,privKeys,None,Some(scriptWit))
  }

  def p2wshOutputs: Gen[Seq[CreditingTxInfo]] = Gen.choose(min,max).flatMap(n => Gen.listOfN(n,p2wshOutput))

  private def build(spk: ScriptPubKey, privKeys: Seq[ECPrivateKey],
                    redeemScript: Option[ScriptPubKey], scriptWitness: Option[ScriptWitness]): Gen[CreditingTxInfo] = nonEmptyOutputs.flatMap { outputs =>
    Gen.choose(0, outputs.size - 1).map { idx =>
      val old = outputs(idx)
      val updated = outputs.updated(idx, TransactionOutput(old.value, spk))
      val tc = TransactionConstants
      val btx = BaseTransaction(tc.version, Nil, updated, tc.lockTime)
      val data = (btx, idx, privKeys, redeemScript, scriptWitness)
      data
    }
  }
}

object CreditingTxGen extends CreditingTxGen
