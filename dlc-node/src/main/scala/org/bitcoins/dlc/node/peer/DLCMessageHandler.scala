package org.bitcoins.dlc.node.peer

import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.protocol.tlv._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** This actor is meant to handle a [[org.bitcoins.core.protocol.tlv.DLCSetupTLV DLCSetupTLV]]
  * that a peer to sent to us on the DLC p2p network, for instance, if we a receive a
  * [[org.bitcoins.core.protocol.tlv.DLCSignTLV DLCSignTLV]] we should store the signatures
  * and then broadcast the funding transaction
  */
case class DLCMessageHandler(dlcWallet: DLCWalletApi)(implicit
    ec: ExecutionContext)
    extends Logging {

  def handlePayload(
      setupTLV: DLCSetupTLV,
      peerMsgSender: PeerMessageSender): Future[DLCMessageHandler] = {

    val resultF = setupTLV match {
      case offerTLV: DLCOfferTLV =>
        for {
          accept <- dlcWallet.acceptDLCOffer(offerTLV)
          _ <- peerMsgSender.sendDLCAcceptMessage(accept)
        } yield this
      case acceptTLV: DLCAcceptTLV =>
        for {
          sign <- dlcWallet.signDLC(acceptTLV)
          _ <- peerMsgSender.sendDLCSignMessage(sign)
        } yield this
      case signTLV: DLCSignTLV =>
        for {
          _ <- dlcWallet.addDLCSigs(signTLV)
          _ <- dlcWallet.broadcastDLCFundingTx(signTLV.contractId)
        } yield this
    }

    resultF.failed.foreach { err =>
      logger.error(s"Failed to handle data payload=$setupTLV", err)
    }

    resultF.recover { case NonFatal(_) =>
      this
    }
  }
}
