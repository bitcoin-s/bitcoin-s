package org.bitcoins.core.gen

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.script.{P2SHScriptPubKey, ScriptPubKey}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionConstants, TransactionOutput}
import org.scalacheck.Gen

sealed abstract class CreditingTxGen {
  /** Minimum amount of outputs to generate */
  private val min = 1
  /** Maximum amount of outputs to generate */
  private val max = 3

  private def nonEmptyOutputs: Gen[Seq[TransactionOutput]] = Gen.choose(1,5).flatMap { n =>
    Gen.listOfN(n, TransactionGenerators.output)
  }

  def nonP2SHOutput: Gen[(Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey])] = Gen.oneOf(p2pkOutput,
    p2pkhOutput, multiSigOutput)

  def output: Gen[(Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey])] = Gen.oneOf(p2pkOutput,
    p2pkhOutput, multiSigOutput, p2shOutput)

  def outputs: Gen[Seq[(Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey])]] = {
    Gen.choose(min,5).flatMap(n => Gen.listOfN(n,output))
  }

  /** Generates a crediting tx with a p2pk spk at the returned index */
  def p2pkOutput: Gen[(Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey])] = ScriptGenerators.p2pkScriptPubKey.flatMap { p2pk =>
    build(p2pk._1,Seq(p2pk._2))
  }
  /** Generates multiple crediting txs with p2pk spks at the returned index */
  def p2pkOutputs: Gen[Seq[(Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey])]] = {
    Gen.choose(min,max).flatMap(n => Gen.listOfN(n,p2pkOutput))
  }

  /** Generates a transaction that has a p2pkh output at the returned index. This
    * output can be spent by the returned ECPrivateKey */
  def p2pkhOutput: Gen[(Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey])] = ScriptGenerators.p2pkhScriptPubKey.flatMap { p2pkh =>
    build(p2pkh._1,Seq(p2pkh._2))
  }

  /** Generates a sequence of p2pkh outputs at the returned index */
  def p2pkhOutputs: Gen[Seq[(Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey])]] = {
    Gen.choose(min,max).flatMap(n => Gen.listOfN(n,p2pkhOutput))
  }

  def multiSigOutput: Gen[(Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey])] = ScriptGenerators.multiSigScriptPubKey.flatMap { multisig =>
    build(multisig._1, multisig._2)
  }

  def multiSigOutputs: Gen[Seq[(Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey])]] = {
    Gen.choose(min,max).flatMap(n => Gen.listOfN(n,multiSigOutput))
  }

  def p2shOutput: Gen[(Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey])] = nonP2SHOutput.map { o =>
    val oldTx = o._1
    val oldOutput = oldTx.outputs(o._2)
    val redeemScript = oldTx.outputs(o._2).scriptPubKey
    val p2sh = P2SHScriptPubKey(redeemScript)
    val updatedOutput = TransactionOutput(oldOutput.value,p2sh)
    val tx = Transaction(oldTx.version, oldTx.inputs, oldTx.outputs.updated(o._2,updatedOutput), oldTx.lockTime)
    (tx,o._2,o._3,Some(redeemScript))
  }

  def p2shOutputs: Gen[Seq[(Transaction, Int, Seq[ECPrivateKey], Option[ScriptPubKey])]] = {
    Gen.choose(min,max).flatMap(n => Gen.listOfN(n,p2shOutput))
  }

  private def build(spk: ScriptPubKey, privKeys: Seq[ECPrivateKey], redeemScript: Option[ScriptPubKey] = None): Gen[(Transaction,Int,Seq[ECPrivateKey], Option[ScriptPubKey])] = nonEmptyOutputs.flatMap { outputs =>
    Gen.choose(0, outputs.size - 1).map { idx =>
      val old = outputs(idx)
      val updated = outputs.updated(idx, TransactionOutput(old.value, spk))
      val tc = TransactionConstants
      val data = (Transaction(tc.version, Nil, updated, tc.lockTime), idx, privKeys, redeemScript)
      data
    }
  }
}

object CreditingTxGen extends CreditingTxGen
