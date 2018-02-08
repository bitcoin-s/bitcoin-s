package org.bitcoins.core.gen

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionConstants, TransactionOutput}
import org.scalacheck.Gen

sealed abstract class CreditingTxGen {
  private def nonEmptyOutputs: Gen[Seq[TransactionOutput]] = Gen.choose(1,5).flatMap { n =>
    Gen.listOfN(n, TransactionGenerators.output)
  }

  /** Generates a crediting tx with a p2pk spk at the returned index */
  def p2pkOutput: Gen[(Transaction, Int, ECPrivateKey)] = ScriptGenerators.p2pkScriptPubKey.flatMap { p2pk =>
    build(p2pk._1,p2pk._2)
  }
  /** Generates multiple crediting txs with p2pk spks at the returned index */
  def p2pkOutputs: Gen[Seq[(Transaction, Int, ECPrivateKey)]] = {
    Gen.choose(1,3).flatMap(n => Gen.listOfN(n,p2pkOutput))
  }

  /** Generates a transaction that has a p2pkh output at the returned index. This
    * output can be spent by the returned ECPrivateKey */
  def p2pkhOutput: Gen[(Transaction, Int, ECPrivateKey)] = ScriptGenerators.p2pkhScriptPubKey.flatMap { p2pkh =>
    build(p2pkh._1,p2pkh._2)
  }

  /** Generates a sequence of p2pkh outputs at the returned index */
  def p2pkhOutputs: Gen[Seq[(Transaction, Int, ECPrivateKey)]] = {
    Gen.choose(1,3).flatMap(n => Gen.listOfN(n,p2pkhOutput))
  }

  private def build(spk: ScriptPubKey, privKey: ECPrivateKey): Gen[(Transaction,Int,ECPrivateKey)] = nonEmptyOutputs.flatMap { outputs =>
    Gen.choose(0, outputs.size - 1).map { idx =>
      val old = outputs(idx)
      val updated = outputs.updated(idx, TransactionOutput(old.value, spk))
      val tc = TransactionConstants
      val data = (Transaction(tc.version, Nil, updated, tc.lockTime), idx, privKey)
      data
    }
  }
}

object CreditingTxGen extends CreditingTxGen
