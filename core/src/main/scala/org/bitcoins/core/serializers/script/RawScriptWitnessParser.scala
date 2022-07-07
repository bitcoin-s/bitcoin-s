package org.bitcoins.core.serializers.script

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script.{
  EmptyScriptWitness,
  ScriptWitness,
  ScriptWitnessV0,
  TaprootWitness
}
import org.bitcoins.core.serializers.RawBitcoinSerializer
import scodec.bits.ByteVector

import scala.annotation.tailrec

/** Created by chris on 12/14/16.
  */
sealed abstract class RawScriptWitnessParser
    extends RawBitcoinSerializer[ScriptWitness] {

  override def read(bytes: ByteVector): ScriptWitness = {
    //first byte is the number of stack items
    val stackSize = CompactSizeUInt.parseCompactSizeUInt(bytes)
    val (_, stackBytes) = bytes.splitAt(stackSize.byteSize.toInt)
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        accum: Seq[ByteVector],
        remainingStackItems: UInt64): Seq[ByteVector] = {
      if (remainingStackItems <= UInt64.zero) accum
      else {
        val elementSize = CompactSizeUInt.parseCompactSizeUInt(remainingBytes)
        val (_, stackElementBytes) =
          remainingBytes.splitAt(elementSize.byteSize.toInt)
        val stackElement = stackElementBytes.take(elementSize.num.toInt)
        val (_, newRemainingBytes) =
          stackElementBytes.splitAt(stackElement.size)
        loop(newRemainingBytes,
             stackElement +: accum,
             remainingStackItems - UInt64.one)
      }
    }
    //note there is no 'reversing' the accum, in bitcoin-s we assume the top of the stack is the 'head' element in the sequence
    val stack = loop(stackBytes, Nil, stackSize.num)
    val witness = ScriptWitness(stack)
    witness
  }

  override def write(scriptWitness: ScriptWitness): ByteVector = {
    @tailrec
    def loop(
        remainingStack: Seq[ByteVector],
        accum: Vector[ByteVector]): Vector[ByteVector] = {
      if (remainingStack.isEmpty) accum.reverse
      else {
        val compactSizeUInt: CompactSizeUInt =
          CompactSizeUInt.calc(remainingStack.head)
        val serialization: ByteVector =
          compactSizeUInt.bytes ++ remainingStack.head
        loop(remainingStack.tail, serialization +: accum)
      }
    }
    val stackWithAnnex = scriptWitness match {
      case _: ScriptWitnessV0 =>
        //no annex in witness v0
        scriptWitness.stack
      case trWit: TaprootWitness =>
        trWit.annexOpt match {
          case Some(annex) =>
            annex +: trWit.stack
          case None =>
            trWit.stack
        }
      case EmptyScriptWitness =>
        EmptyScriptWitness.stack
    }
    val stackItems: Vector[ByteVector] =
      loop(stackWithAnnex.reverse, Vector.empty)
    val size = CompactSizeUInt(UInt64(stackItems.size))
    (size.bytes +: stackItems).fold(ByteVector.empty)(_ ++ _)
  }
}

object RawScriptWitnessParser extends RawScriptWitnessParser
