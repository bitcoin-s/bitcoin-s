package org.bitcoins.core.protocol.transaction

import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

/** Represents a generic on-chain output */
case class OutputReference(
    outPoint: TransactionOutPoint,
    output: TransactionOutput)
    extends NetworkElement {
  override def bytes: ByteVector = {
    outPoint.bytes ++ output.bytes
  }
}

object EmptyOutputReference
    extends OutputReference(EmptyTransactionOutPoint, EmptyTransactionOutput) {
  override def toString: String = "EmptyOutputReference"
}

object OutputReference extends Factory[OutputReference] {
  override def fromBytes(bytes: ByteVector): OutputReference = {
    val (outPointBytes, outputBytes) = bytes.splitAt(36)
    val outPoint = TransactionOutPoint(outPointBytes)
    val output = TransactionOutput(outputBytes)

    OutputReference(outPoint, output)
  }
}
