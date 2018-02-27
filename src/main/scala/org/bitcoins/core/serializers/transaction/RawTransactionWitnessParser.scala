package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.protocol.script.ScriptWitness
import org.bitcoins.core.protocol.transaction.TransactionWitness
import org.bitcoins.core.serializers.script.RawScriptWitnessParser

import scala.annotation.tailrec

/**
  * Created by chris on 11/21/16.
  * Serialization of [[TransactionWitness]] as defined inside of BIP141
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#specification]]
  * [[https://github.com/bitcoin/bitcoin/blob/b4e4ba475a5679e09f279aaf2a83dcf93c632bdb/src/primitives/transaction.h#L232-L268]]
  */
sealed abstract class RawTransactionWitnessParser {

  /** We can only tell how many [[ScriptWitness]]
    * we have if we have the number of inputs the transaction creates
    */
  def read(bytes: Seq[Byte], numInputs: Int): TransactionWitness = {
    @tailrec
    def loop(remainingBytes: Seq[Byte], remainingInputs: Int, accum: Seq[ScriptWitness]): Seq[ScriptWitness] = {
      if (remainingInputs != 0) {
        val w = RawScriptWitnessParser.read(remainingBytes)
        val (_,newRemainingBytes) = remainingBytes.splitAt(w.bytes.size)
        loop(newRemainingBytes,remainingInputs - 1, w +: accum)
      } else accum.reverse
    }
    val witnesses = loop(bytes,numInputs,Nil)
    require(witnesses.size == numInputs)
    TransactionWitness(witnesses)
  }

  def write(witness: TransactionWitness): Seq[Byte] = witness.witnesses.flatMap(_.bytes)
}

object RawTransactionWitnessParser extends RawTransactionWitnessParser