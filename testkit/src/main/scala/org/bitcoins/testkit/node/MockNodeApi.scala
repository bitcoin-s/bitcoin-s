package org.bitcoins.testkit.node

import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.Future

object MockNodeApi extends NodeApi {
  val mock: NodeApi = this

  override def broadcastTransactions(
      transactions: Vector[Transaction]
  ): Future[Unit] =
    Future.unit

  override def downloadBlocks(
      blockHashes: Vector[DoubleSha256DigestBE]
  ): Future[Unit] = Future.unit

  override def getConnectionCount: Future[Int] = Future.successful(0)

}
