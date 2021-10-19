package com.bitcoins.clightning.rpc

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.script.P2WSHWitnessSPKV0
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.clightning.CLightningRpcTestUtil
import org.bitcoins.testkit.fixtures.CLightningChannelOpenerFixture

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class CLightningChannelOpenerTest extends CLightningChannelOpenerFixture {

  it must "get channel funding info" in { param =>
    val (_, clightningA, clightningB) = param
    val amount = Satoshis(100000)

    for {
      _ <- CLightningRpcTestUtil.connectLNNodes(clightningA, clightningB)
      nodeId <- clightningB.nodeId
      fundDetails <- clightningA.initChannelOpen(nodeId = nodeId,
                                                 amount = amount,
                                                 privateChannel = false)
    } yield {
      assert(
        fundDetails.funding_address.scriptPubKey
          .isInstanceOf[P2WSHWitnessSPKV0])
    }
  }

  it must "open a channel with external funding" in { param =>
    val (bitcoind, clightningA, clightningB) = param
    val amount = Satoshis(100000)

    for {
      _ <- CLightningRpcTestUtil.connectLNNodes(clightningA, clightningB)
      preChannelsA <- clightningA.listChannels()
      preChannelsB <- clightningB.listChannels()
      _ = assert(preChannelsA.isEmpty)
      _ = assert(preChannelsB.isEmpty)

      nodeId <- clightningB.nodeId
      fundDetails <- clightningA.initChannelOpen(nodeId = nodeId,
                                                 amount = amount,
                                                 privateChannel = false)

      // construct psbt
      psbt <- bitcoind
        .walletCreateFundedPsbt(Vector.empty,
                                Map(fundDetails.funding_address -> amount))
        .map(_.psbt)
      // fund channel with psbt
      _ <- clightningA.completeChannelOpen(nodeId, psbt)

      midChannelsA <- clightningA.listChannels()
      midChannelsB <- clightningB.listChannels()
      _ = assert(midChannelsA.isEmpty)
      _ = assert(midChannelsB.isEmpty)

      res <- bitcoind.walletProcessPSBT(psbt)
      tx <- Future.fromTry(res.psbt.extractTransactionAndValidate)
      _ <- bitcoind.sendRawTransaction(tx)
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))

      // await for clightnings to see channel
      _ <- TestAsyncUtil.awaitConditionF(
        () => clightningA.listChannels().map(_.nonEmpty),
        interval = 1.second,
        maxTries = 500)
      _ <- TestAsyncUtil.awaitConditionF(
        () => clightningB.listChannels().map(_.nonEmpty),
        interval = 1.second,
        maxTries = 500)
    } yield succeed
  }

  it must "cancel a channel" in { param =>
    val (bitcoind, clightningA, clightningB) = param
    val amount = Satoshis(100000)

    for {
      _ <- CLightningRpcTestUtil.connectLNNodes(clightningA, clightningB)
      preChannelsA <- clightningA.listChannels()
      preChannelsB <- clightningB.listChannels()
      _ = assert(preChannelsA.isEmpty)
      _ = assert(preChannelsB.isEmpty)

      nodeId <- clightningB.nodeId
      fundDetails <- clightningA.initChannelOpen(nodeId = nodeId,
                                                 amount = amount,
                                                 privateChannel = false)

      // construct psbt
      psbt <- bitcoind
        .walletCreateFundedPsbt(Vector.empty,
                                Map(fundDetails.funding_address -> amount))
        .map(_.psbt)
      // fund channel with psbt
      _ <- clightningA.completeChannelOpen(nodeId, psbt)

      midChannelsA <- clightningA.listChannels()
      midChannelsB <- clightningB.listChannels()
      _ = assert(midChannelsA.isEmpty)
      _ = assert(midChannelsB.isEmpty)

      // cancel channel
      _ <- clightningA.cancelChannelOpen(nodeId)

      res <- bitcoind.walletProcessPSBT(psbt)
      tx <- Future.fromTry(res.psbt.extractTransactionAndValidate)
      _ <- bitcoind.sendRawTransaction(tx)
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))

      // await for clightning to see channel
      _ <- TestAsyncUtil.awaitConditionF(
        () => clightningA.listChannels().map(_.isEmpty),
        interval = 1.second,
        maxTries = 500)
      _ <- TestAsyncUtil.awaitConditionF(
        () => clightningB.listChannels().map(_.isEmpty),
        interval = 1.second,
        maxTries = 500)
    } yield succeed
  }
}
