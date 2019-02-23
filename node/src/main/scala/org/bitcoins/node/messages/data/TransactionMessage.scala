package org.bitcoins.node.messages.data

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.Factory
import org.bitcoins.node.messages.TransactionMessage
import org.bitcoins.node.serializers.messages.data.RawTransactionMessageSerializer
import org.bitcoins.node.messages._
import org.bitcoins.node.serializers.messages.data.RawTransactionMessageSerializer
import scodec.bits.ByteVector

/**
  * Created by chris on 6/2/16.
  * Companion factory object for the TransactionMessage on the p2p network
  * https://bitcoin.org/en/developer-reference#tx
  */
object TransactionMessage extends Factory[TransactionMessage] {

  private case class TransactionMessageImpl(transaction: Transaction)
      extends TransactionMessage

  def fromBytes(bytes: ByteVector): TransactionMessage =
    RawTransactionMessageSerializer.read(bytes)

  def apply(transaction: Transaction): TransactionMessage =
    TransactionMessageImpl(transaction)
}
