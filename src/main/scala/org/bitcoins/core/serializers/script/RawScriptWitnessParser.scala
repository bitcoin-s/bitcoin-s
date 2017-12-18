package org.bitcoins.core.serializers.script

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script.ScriptWitness
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSUtil

import scala.annotation.tailrec

/**
  * Created by chris on 12/14/16.
  */
trait RawScriptWitnessParser extends RawBitcoinSerializer[ScriptWitness] {

  def read(bytes: List[Byte]): ScriptWitness = {
    logger.debug("Bytes for witness: " + BitcoinSUtil.encodeHex(bytes))
    //first byte is the number of stack items
    val stackSize = CompactSizeUInt.parseCompactSizeUInt(bytes)
    logger.debug("Stack size: " + stackSize)
    val (_,stackBytes) = bytes.splitAt(stackSize.size.toInt)
    logger.debug("Stack bytes: " + BitcoinSUtil.encodeHex(stackBytes))
    @tailrec
    def loop(remainingBytes: Seq[Byte], accum: Seq[Seq[Byte]], remainingStackItems: UInt64): Seq[Seq[Byte]] = {
      if (remainingStackItems <= UInt64.zero) accum
      else {
        val elementSize = CompactSizeUInt.parseCompactSizeUInt(remainingBytes)
        val (_,stackElementBytes) = remainingBytes.splitAt(elementSize.size.toInt)
        val stackElement = stackElementBytes.take(elementSize.num.toInt)
        logger.debug("Parsed stack element: " + BitcoinSUtil.encodeHex(stackElement))
        val (_,newRemainingBytes) = stackElementBytes.splitAt(stackElement.size)
        logger.debug("New remaining bytes: " + BitcoinSUtil.encodeHex(newRemainingBytes))
        loop(newRemainingBytes, stackElement +: accum, remainingStackItems - UInt64.one)
      }
    }
    //note there is no 'reversing' the accum, in bitcoin-s we assume the top of the stack is the 'head' element in the sequence
    val stack = loop(stackBytes,Nil,stackSize.num)
    val witness = ScriptWitness(stack)
    witness
  }

  def write(scriptWitness: ScriptWitness): Seq[Byte] = {
    @tailrec
    def loop(remainingStack: Seq[Seq[Byte]], accum: Seq[Seq[Byte]]): Seq[Seq[Byte]] = {
      if (remainingStack.isEmpty) accum.reverse
      else {
        val compactSizeUInt: CompactSizeUInt = CompactSizeUInt.calc(remainingStack.head)
        val serialization: Seq[Byte] = compactSizeUInt.bytes ++ remainingStack.head
        loop(remainingStack.tail, serialization +: accum)
      }
    }
    val stackItems: Seq[Seq[Byte]] = loop(scriptWitness.stack.reverse,Nil)
    val size = CompactSizeUInt(UInt64(stackItems.size))
    (size.bytes +: stackItems).flatten
  }
}

object RawScriptWitnessParser extends RawScriptWitnessParser
