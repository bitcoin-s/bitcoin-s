package org.bitcoins.rpc

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.CallbackHandler
import org.bitcoins.core.api.callback.{
  CallbackFactory,
  ModuleCallbacks,
  NodeApiCallbacks,
  OnBlockReceived
}
import org.bitcoins.core.protocol.blockchain.Block

trait BitcoindCallbacks
    extends NodeApiCallbacks
    with ModuleCallbacks[BitcoindCallbacks]
    with BitcoinSLogger

object BitcoindCallbacks extends CallbackFactory[BitcoindCallbacks] {
  private case class BitcoindCallbacksImpl(
      onBlockReceived: CallbackHandler[Block, OnBlockReceived])
      extends BitcoindCallbacks {
    override def +(other: BitcoindCallbacks): BitcoindCallbacks = {
      copy(onBlockReceived = onBlockReceived ++ other.onBlockReceived)
    }
  }
  override def empty: BitcoindCallbacks = BitcoindCallbacksImpl(
    CallbackHandler.empty)
}
