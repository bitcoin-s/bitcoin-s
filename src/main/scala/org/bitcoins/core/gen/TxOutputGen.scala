package org.bitcoins.core.gen

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.scalacheck.Gen

sealed abstract class TxOutputGen {

  /** Generates a transaction that has a p2pkh output at the returned index. This
    * output can be spent by the returned ECPrivateKey */
  def p2pkhOutput: Gen[(Transaction, Int, ECPrivateKey)] = TransactionGenerators.transaction.flatMap { tx =>
    ScriptGenerators.p2pkhScriptPubKey.flatMap { p2pkh =>
      Gen.choose(0,tx.outputs.size-1).map { idx =>
        val old = tx.outputs(idx)
        val outputs = tx.outputs.updated(idx, TransactionOutput(old.value,p2pkh._1))
        (Transaction(tx.version,tx.inputs,outputs,tx.lockTime),idx, p2pkh._2)
      }
    }
  }

  def p2pkhOutputs: Gen[Seq[(Transaction, Int, ECPrivateKey)]] = {
    Gen.choose(1,3).flatMap(n => Gen.listOfN(n,p2pkhOutput))

  }
}

object TxOutputGen extends TxOutputGen
