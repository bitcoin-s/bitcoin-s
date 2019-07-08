package org.bitcoins.node.networking.peer
import org.bitcoins.core.p2p.Inventory
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.p2p.TypeIdentifier
import org.bitcoins.core.protocol.blockchain.Block

/** Type class for things that can be sent as part
  * of a P2P inventory message
  */
trait InventoryEsque[T] {
  def toInv(elem: T): Inventory

}

/** Default implementations of things that can be sent as
  * a inventory message
  */
object InventoryEsque {

  implicit val transactionInventoryEsque = new InventoryEsque[Transaction] {

    def toInv(elem: Transaction): Inventory =
      Inventory(TypeIdentifier.MsgTx, elem.txId)
  }

  implicit val blockInventoryEsque = new InventoryEsque[Block] {

    def toInv(elem: Block): Inventory =
      Inventory(TypeIdentifier.MsgBlock, elem.blockHeader.hash)
  }
}
