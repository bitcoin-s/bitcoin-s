package org.bitcoins.node.models

import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape
import org.bitcoins.db.DbRowAutoInc
import org.bitcoins.db.TableAutoInc
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import scodec.bits.ByteVector
import org.bitcoins.core.protocol.transaction.Transaction

/** TXs we can broadcast over the P2P network */
final case class BroadcastAbleTransaction(
    transaction: Transaction,
    id: Option[Long] = None)
    extends DbRowAutoInc[BroadcastAbleTransaction] {
  def copyWithId(id: Long): BroadcastAbleTransaction = copy(id = Some(id))
}

/** Table over TXs we can broadcast over the P2P network */
final case class BroadcastAbleTransactionTable(tag: Tag)
    extends TableAutoInc[BroadcastAbleTransaction](tag, "broadcast_elements") {
  private type Tuple = (DoubleSha256DigestBE, ByteVector, Option[Long])

  private val fromTuple: (Tuple => BroadcastAbleTransaction) = {
    case (txid, bytes, id) =>
      val tx = Transaction.fromBytes(bytes)
      require(tx.txId == txid.flip)
      BroadcastAbleTransaction(tx, id)
  }

  private val toTuple: BroadcastAbleTransaction => Option[Tuple] = tx =>
    Some(tx.transaction.txId.flip, tx.transaction.bytes, tx.id)

  import org.bitcoins.db.DbCommonsColumnMappers._

  def txid: Rep[DoubleSha256DigestBE] = column("txid", O.Unique)
  def bytes: Rep[ByteVector] = column("tx_bytes")

  def * : ProvenShape[BroadcastAbleTransaction] =
    (txid, bytes, id.?) <>
      (fromTuple, toTuple)

}
