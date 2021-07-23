package org.bitcoins.node.networking.peer

import org.bitcoins.core.currency.BitcoinsInt
import org.bitcoins.core.p2p.TransactionMessage
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.node.OnTxReceived
import org.bitcoins.node.NodeCallbacks
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.{
  NeutrinoNodeFundedWalletBitcoind,
  NodeTestWithCachedBitcoindNewest
}
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.{Future, Promise}

class DataMessageHandlerNeutrinoNodesTest
    extends NodeTestWithCachedBitcoindNewest {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNodeFundedWalletBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val outcomeF: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withNeutrinoNodeFundedWalletBitcoind(
        test = test,
        bip39PasswordOpt = getBIP39PasswordOpt(),
        bitcoind = bitcoind
      )(system, getFreshConfig)
      f <- outcome.toFuture
    } yield f

    new FutureOutcome(outcomeF)
  }

  it must "verify OnTxReceived callbacks are executed" in {
    param: FixtureParam =>
      val NeutrinoNodeFundedWalletBitcoind(node, _, bitcoind, _) = param

      val resultP: Promise[Transaction] = Promise()

      val callback: OnTxReceived = (tx: Transaction) => {
        Future {
          resultP.success(tx)
          ()
        }
      }
      val sender = node.peerMsgSenders(0)

      for {

        txId <- bitcoind.sendToAddress(junkAddress, 1.bitcoin)
        tx <- bitcoind.getRawTransactionRaw(txId)

        payload = TransactionMessage(tx)

        nodeCallbacks = NodeCallbacks.onTxReceived(callback)
        _ = node.nodeAppConfig.addCallbacks(nodeCallbacks)

        dataMessageHandler =
          DataMessageHandler(genesisChainApi)(node.executionContext,
                                              node.nodeAppConfig,
                                              node.chainConfig)
        _ <- dataMessageHandler.handleDataPayload(payload, sender)
        result <- resultP.future
      } yield assert(result == tx)
  }
}
