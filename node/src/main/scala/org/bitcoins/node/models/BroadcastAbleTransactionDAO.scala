package org.bitcoins.node.models

import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.node.config.NodeAppConfig

import scala.concurrent.ExecutionContext
import slick.lifted.ProvenShape

import scala.concurrent.Future
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import scodec.bits.ByteVector

final case class BroadcastAbleTransactionDAO()(
    implicit override val appConfig: NodeAppConfig,
    val ec: ExecutionContext)
    extends CRUDAutoInc[BroadcastAbleTransaction] {

  import profile.api._

  override val table: profile.api.TableQuery[BroadcastAbleTransactionTable] =
    profile.api.TableQuery[BroadcastAbleTransactionTable]

  /** Searches for a TX by its TXID */
  def findByHash(
      hash: DoubleSha256Digest): Future[Option[BroadcastAbleTransaction]] = {
    import org.bitcoins.db.DbCommonsColumnMappers._

    val query = table.filter(_.txid === hash.flip)
    database.run(query.result).map(_.headOption)
  }

  /** Table over TXs we can broadcast over the P2P network */
  class BroadcastAbleTransactionTable(tag: Tag)
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

}
