package org.bitcoins.node.networking

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.event.LoggingReceive
import akka.io.Tcp
import org.bitcoins.core.bloom.{BloomFilter, BloomUpdateNone}
import org.bitcoins.core.crypto.{DoubleSha256Digest, Sha256Hash160Digest}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.Address
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.messages._
import org.bitcoins.node.messages.control.FilterLoadMessage
import org.bitcoins.node.messages.data.{GetDataMessage, Inventory}
import org.bitcoins.node.util.BitcoinSpvNodeUtil

/**
  * Created by chris on 8/30/16.
  * Responsible for checking if a payment to a address was made
  * Verifying that the transaction that made the payment was included
  * inside of a block on the blockchain
  *
  * 1.) Creates a bloom filter
  * 2.) Sends the bloom filter to a node on the network
  * 3.) Nodes matches the bloom filter, sends a txid that matched the filter back to us
  * 4.) We request the full transaction using a [[GetDataMessage]]
  * 5.) We verify the transaction given to us has an output that matches the address we expected a payment to
  * 6.) When another block is announced on the network, we send a MsgMerkleBlock
  * to our peer on the network to see if the tx was included on that block
  * 7.) If it was, send the actor that that requested this message back
  */
sealed abstract class PaymentActor extends Actor with BitcoinSLogger {

  def peerMsgHandler: ActorRef

  def receive = LoggingReceive {
    case hash: Sha256Hash160Digest =>
      paymentToHash(hash)
    case address: Address =>
      self.forward(address.hash)
  }

  /** Constructs a bloom filter that matches the given hash,
    * then sends that bloom filter to a peer on the network */
  def paymentToHash(hash: Sha256Hash160Digest) = {
    val bloomFilter =
      BloomFilter(10, 0.0001, UInt32.zero, BloomUpdateNone).insert(hash)
    val filterLoadMsg = FilterLoadMessage(bloomFilter)
    val bloomFilterNetworkMsg =
      NetworkMessage(Constants.networkParameters, filterLoadMsg)
    peerMsgHandler ! bloomFilterNetworkMsg
  }

  /** Awaits for a [[GetDataMessage]] that requested a transaction. We can also fire off more [[GetDataMessage]] inside of this context */
  def awaitTransactionGetDataMessage(
      hash: Sha256Hash160Digest,
      peerMessageHandler: ActorRef): Receive = LoggingReceive {
    case txMsg: TransactionMessage =>
      //check to see if any of the outputs on this tx match our hash
      val outputs = txMsg.transaction.outputs.filter(o =>
        o.scriptPubKey.asm.filter(_.bytes == hash.bytes).nonEmpty)

      if (outputs.nonEmpty) {
        logger.debug(
          "matched transaction inside of awaitTransactionGetDataMsg: " + txMsg.transaction.hex)
        logger.debug("Matched txid: " + txMsg.transaction.txId.hex)
        logger.debug("Switching to awaitBlockAnnouncement")
        context.become(
          awaitBlockAnnouncement(hash,
                                 txMsg.transaction.txId,
                                 peerMessageHandler))
      }
    //otherwise we do nothing and wait for another transaction message
    case invMsg: InventoryMessage =>
      //txs are broadcast by nodes on the network when they are seen by a node
      //filter out the txs we do not care about
      val txInventories =
        invMsg.inventories.filter(_.typeIdentifier == TypeIdentifier.MsgTx)
      handleTransactionInventoryMessages(txInventories, peerMessageHandler)
  }

  /** Sends a [[GetDataMessage]] to get the full transaction for a transaction inventory message */
  private def handleTransactionInventoryMessages(
      inventory: Seq[Inventory],
      peerMessageHandler: ActorRef): Unit = {
    for {
      txInv <- inventory
      inventory = GetDataMessage(txInv)
    } yield peerMessageHandler ! inventory

    ()
  }

  /** This context waits for a block announcement on the network,
    * then constructs a [[MerkleBlockMessage]] to check
    * if the txid was included in that block */
  def awaitBlockAnnouncement(
      hash: Sha256Hash160Digest,
      txId: DoubleSha256Digest,
      peerMessageHandler: ActorRef): Receive = LoggingReceive {
    case invMsg: InventoryMessage =>
      val blockHashes =
        invMsg.inventories
          .filter(_.typeIdentifier == TypeIdentifier.MsgBlock)
          .map(_.hash)
      if (blockHashes.nonEmpty) {
        //construct a merkle block message to verify that the txIds was in the block
        val merkleBlockInventory =
          Inventory(TypeIdentifier.MsgFilteredBlock, blockHashes.head)
        val getDataMsg = GetDataMessage(merkleBlockInventory)
        val getDataNetworkMessage =
          NetworkMessage(Constants.networkParameters, getDataMsg)
        peerMessageHandler ! getDataNetworkMessage
        logger.debug("Switching to awaitMerkleBlockMessage")
        context.become(
          awaitMerkleBlockMessage(hash, txId, blockHashes, peerMessageHandler))
      }
    //else do nothing and wait for another block announcement

  }

  /** This context waits for a [[MerkleBlockMessage]] from our peer on the network, then checks
    * if the given txid is contained inside of the block. If it is included, send a [[PaymentActor.SuccessfulPayment]]
    * message back to the actor that created this actor, else send a [[PaymentActor.FailedPayment]] message back to
    * the actor that created this actor
    * @param hash
    * @param txId
    * @param blockHashes
    * @param peerMessageHandler
    * @return
    */
  def awaitMerkleBlockMessage(
      hash: Sha256Hash160Digest,
      txId: DoubleSha256Digest,
      blockHashes: Seq[DoubleSha256Digest],
      peerMessageHandler: ActorRef): Receive = LoggingReceive {
    case merkleBlockMsg: MerkleBlockMessage =>
      val result = merkleBlockMsg.merkleBlock.partialMerkleTree.extractMatches
        .contains(txId)
      if (result) {
        val successfulPayment =
          PaymentActor.SuccessfulPayment(hash,
                                         txId,
                                         blockHashes,
                                         merkleBlockMsg.merkleBlock)
        logger.info("Received successful payment: " + successfulPayment)
        context.parent ! successfulPayment
      } else context.parent ! PaymentActor.FailedPayment(hash)
      peerMessageHandler ! Tcp.Close
      context.stop(self)
  }
}

object PaymentActor {
  private case class PaymentActorImpl(peerMsgHandler: ActorRef)
      extends PaymentActor

  def props(peerMsgHandler: ActorRef): Props =
    Props(classOf[PaymentActorImpl], peerMsgHandler)

  def apply(peerMsgHandler: ActorRef)(
      implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(peerMsgHandler),
                    BitcoinSpvNodeUtil.createActorName(this.getClass))

  sealed trait PaymentActorMessage
  case class SuccessfulPayment(
      hash: Sha256Hash160Digest,
      txId: DoubleSha256Digest,
      blockHash: Seq[DoubleSha256Digest],
      merkleBlock: MerkleBlock)
      extends PaymentActorMessage

  case class FailedPayment(hash: Sha256Hash160Digest)
      extends PaymentActorMessage
}
