package org.bitcoins.node.serializers.messages

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.node.messages.TransactionMessage
import scodec.bits.ByteVector

/**
  * Responsible for serializing and deserializing TransactionMessage network objects
  * @see https://bitcoin.org/en/developer-reference#tx
  */
trait RawTransactionMessageSerializer
    extends RawBitcoinSerializer[TransactionMessage] {

  def read(bytes: ByteVector): TransactionMessage = {
    val transaction = Transaction(bytes)
    TransactionMessage(transaction)
  }

  def write(transactionMessage: TransactionMessage): ByteVector = {
    transactionMessage.transaction.bytes
  }
}

object RawTransactionMessageSerializer extends RawTransactionMessageSerializer
