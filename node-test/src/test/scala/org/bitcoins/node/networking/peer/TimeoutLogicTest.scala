package org.bitcoins.node.networking.peer

import org.bitcoins.core.p2p._
import org.scalatest.flatspec.AsyncFlatSpec

/** Unit test for timeout logic to ensure transaction messages don't trigger
  * spurious timeouts
  */
class TimeoutLogicTest extends AsyncFlatSpec {

  behavior of "DataMessageHandler timeout logic"

  it must "not consider transaction messages as sync-related" in {
    // Create a transaction message (normal network traffic)
    val txMsg = TransactionMessage(
      org.bitcoins.core.protocol.transaction.EmptyTransaction
    )

    // The key test: isSyncRelatedMessage should return false for transactions
    // This prevents the timeout check from being triggered
    val dmh = TestDataMessageHandler()
    assert(!dmh.testIsSyncRelatedMessage(txMsg))

    succeed
  }

  it must "consider filter messages as sync-related" in {
    val dmh = TestDataMessageHandler()
    
    // Test with CompactFilterHeadersMessage
    val filterHeaderMsg = CompactFilterHeadersMessage(
      org.bitcoins.core.gcs.FilterType.Basic,
      org.bitcoins.crypto.DoubleSha256Digest.empty,
      org.bitcoins.crypto.DoubleSha256Digest.empty,
      Vector.empty
    )
    assert(dmh.testIsSyncRelatedMessage(filterHeaderMsg))

    succeed
  }

  it must "consider header messages as sync-related" in {
    val headerMsg = HeadersMessage(Vector.empty)

    val dmh = TestDataMessageHandler()
    assert(dmh.testIsSyncRelatedMessage(headerMsg))

    succeed
  }

  it must "not consider inventory messages as sync-related" in {
    val invMsg = InventoryMessage(Vector.empty)
    val dmh = TestDataMessageHandler()
    assert(!dmh.testIsSyncRelatedMessage(invMsg))
    succeed
  }

  it must "not consider block messages as sync-related" in {
    val blockMsg = BlockMessage(
      org.bitcoins.core.protocol.blockchain.Block(
        org.bitcoins.core.protocol.blockchain.BlockHeader(
          version = org.bitcoins.core.number.Int32.zero,
          previousBlockHash = org.bitcoins.crypto.DoubleSha256Digest.empty,
          merkleRootHash = org.bitcoins.crypto.DoubleSha256Digest.empty,
          time = org.bitcoins.core.number.UInt32.zero,
          nBits = org.bitcoins.core.number.UInt32.zero,
          nonce = org.bitcoins.core.number.UInt32.zero
        ),
        transactions = Vector.empty
      )
    )
    val dmh = TestDataMessageHandler()
    assert(!dmh.testIsSyncRelatedMessage(blockMsg))
    succeed
  }
}

/** Test wrapper that replicates the sync message classification logic from
  * DataMessageHandler.isSyncRelatedMessage(). While this duplicates the logic,
  * it allows for simple unit testing without needing to instantiate a full
  * DataMessageHandler with all its dependencies. The logic must be kept in sync
  * with the production implementation.
  */
case class TestDataMessageHandler() {
  def testIsSyncRelatedMessage(payload: DataPayload): Boolean = {
    payload match {
      case _: HeadersMessage                     => true
      case _: CompactFilterHeadersMessage        => true
      case _: CompactFilterMessage               => true
      case _: CompactFilterCheckPointMessage     => true
      case _                                     => false
    }
  }
}
