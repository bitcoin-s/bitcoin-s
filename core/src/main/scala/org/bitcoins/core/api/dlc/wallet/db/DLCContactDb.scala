package org.bitcoins.core.api.dlc.wallet.db

import org.bitcoins.core.config.DLC
import org.bitcoins.core.util.NetworkUtil

import java.net.InetSocketAddress

case class DLCContactDb(alias: String, address: InetSocketAddress, memo: String)

object DLCContactDbHelper {

  def fromPeerAddress(peerAddress: String): DLCContactDb =
    DLCContactDb(
      alias = "",
      address =
        NetworkUtil.parseInetSocketAddress(peerAddress, DLC.DefaultPort),
      memo = ""
    )

}
