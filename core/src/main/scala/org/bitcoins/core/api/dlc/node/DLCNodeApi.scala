package org.bitcoins.core.api.dlc.node

import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.StartStopAsync

import java.net.InetSocketAddress
import scala.concurrent.Future

trait DLCNodeApi extends StartStopAsync[Unit] {

  def wallet: DLCWalletApi

  def acceptDLCOffer(
      peerAddress: InetSocketAddress,
      dlcOffer: LnMessage[DLCOfferTLV]): Future[Unit]

  def getHostAddress: Future[InetSocketAddress]
}
