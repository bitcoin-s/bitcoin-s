package org.bitcoins.rpc.callback

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.CallbackHandler
import org.bitcoins.core.api.callback.{
  CallbackFactory,
  NodeApiCallbacks,
  OnBlockReceived,
  OnTxReceived
}
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction

trait BitcoindCallbacks
    extends NodeApiCallbacks[BitcoindCallbacks]
    with BitcoinSLogger

object BitcoindCallbacks extends CallbackFactory[BitcoindCallbacks] {
  private case class BitcoindCallbacksImpl(
      onBlockReceived: CallbackHandler[Block, OnBlockReceived],
      onTxReceived: CallbackHandler[Transaction, OnTxReceived])
      extends BitcoindCallbacks {
    override def +(other: BitcoindCallbacks): BitcoindCallbacks = {
      copy(onBlockReceived = onBlockReceived ++ other.onBlockReceived)
    }
  }
  override def empty: BitcoindCallbacks =
    BitcoindCallbacksImpl(CallbackHandler.empty, CallbackHandler.empty)

  /** Constructs a set of callbacks that only acts on block received */
  def onBlockReceived(f: OnBlockReceived): BitcoindCallbacks = {
    BitcoindCallbacksImpl(onBlockReceived =
                            CallbackHandler[Block, OnBlockReceived](
                              "onBlockReceived",
                              Vector(f)
                            ),
                          CallbackHandler.empty)
  }

  def onTxReceived(f: OnTxReceived): BitcoindCallbacks = {
    BitcoindCallbacksImpl(onBlockReceived = CallbackHandler.empty,
                          onTxReceived =
                            CallbackHandler("onTransactionReceived", Vector(f)))
  }

  def apply(
      onBlockReceived: Vector[OnBlockReceived],
      onTxReceived: Vector[OnTxReceived]): BitcoindCallbacks = {
    BitcoindCallbacksImpl(
      CallbackHandler("onBlockReceived", onBlockReceived),
      CallbackHandler("onTxReceived", onTxReceived)
    )
  }
}
