package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script.ScriptWitness
import org.bitcoins.core.protocol.transaction.TransactionInputWitness
import org.bitcoins.core.script.constant.{ScriptConstant, ScriptToken}
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSUtil

import scala.annotation.tailrec

/**
  * Created by chris on 11/21/16.
  */
trait RawTransactionInputWitnessParser extends RawBitcoinSerializer[TransactionInputWitness] {

  override def read(bytes: List[Byte]): TransactionInputWitness = {
    //first byte is the number of stack items
    val stackSize = CompactSizeUInt.parseCompactSizeUInt(bytes)
    val (_,stackBytes) = bytes.splitAt(stackSize.size.toInt)
    @tailrec
    def loop(remainingBytes: Seq[Byte], accum: Seq[Seq[Byte]], remainingStackItems: UInt64): Seq[Seq[Byte]] = {
      if (remainingStackItems <= UInt64.zero) accum.reverse
      else {
        val elementSize = CompactSizeUInt.parseCompactSizeUInt(remainingBytes)
        val (_,stackElement) = remainingBytes.splitAt(elementSize.size.toInt)
        val (_,newRemainingBytes) = remainingBytes.splitAt(elementSize.size.toInt + stackElement.size)
        loop(newRemainingBytes, stackElement +: accum, remainingStackItems - UInt64.one)
      }
    }
    val stack = loop(stackBytes,Nil,stackSize.num)
    val witness = ScriptWitness(stack)
    TransactionInputWitness(witness)
  }


  override def write(txInputWitness: TransactionInputWitness): String = {
    @tailrec
    def loop(remainingStack: Seq[Seq[Byte]], accum: Seq[String]): String = {
      if (remainingStack.isEmpty) accum.reverse.mkString
      else {
        val compactSizeUInt: CompactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(remainingStack.head)
        val serialization: Seq[Byte] = compactSizeUInt.bytes ++ remainingStack.head
        loop(remainingStack.tail, BitcoinSUtil.encodeHex(serialization) +: accum)
      }
    }
    val stackItems: String = loop(txInputWitness.witness.stack,Nil)
    val size = CompactSizeUInt.calculateCompactSizeUInt(stackItems)
    size.hex + stackItems
  }
}

object RawTransactionInputWitnessParser extends RawTransactionInputWitnessParser