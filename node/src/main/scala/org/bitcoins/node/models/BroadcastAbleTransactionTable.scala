package org.bitcoins.node.models

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.db.DbRowAutoInc

/** TXs we can broadcast over the P2P network */
final case class BroadcastAbleTransaction(
    transaction: Transaction,
    id: Option[Long] = None)
    extends DbRowAutoInc[BroadcastAbleTransaction] {
  def copyWithId(id: Long): BroadcastAbleTransaction = copy(id = Some(id))
}
