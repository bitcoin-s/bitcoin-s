package org.bitcoins.core.api.dlc.node

import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.models.DLCMessage
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.crypto.Sha256Digest

import java.net.InetSocketAddress
import scala.concurrent.Future

trait DLCNodeApi extends StartStopAsync[Unit] {

  def wallet: DLCWalletApi

  def acceptDLCOffer(
      peerAddress: InetSocketAddress,
      dlcOffer: LnMessage[DLCOfferTLV],
      externalPayoutAddress: Option[BitcoinAddress],
      externalChangeAddress: Option[BitcoinAddress]): Future[
    DLCMessage.DLCAccept]

  def sendDLCOffer(
      peerAddress: InetSocketAddress,
      incomingOfferHash: Sha256Digest): Future[Unit]

  def getHostAddress: Future[InetSocketAddress]
}
