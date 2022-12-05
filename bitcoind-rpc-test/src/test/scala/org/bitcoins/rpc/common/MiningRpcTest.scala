package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.{
  GetBlockWithTransactionsResultV22,
  RpcOpts
}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.concurrent.Future

class MiningRpcTest extends BitcoindRpcTest {

  lazy val clientsF: Future[(BitcoindRpcClient, BitcoindRpcClient)] =
    BitcoindRpcTestUtil.createNodePair(clientAccum = clientAccum)

  behavior of "MiningRpc"

  it should "be able to get a block template" in {
    clientsF.flatMap { case (client, _) =>
      val opts =
        RpcOpts.BlockTemplateRequest(
          mode = "template",
          capabilities = Vector.empty,
          rules = Vector("segwit")
        )
      val getBlockF = client.getBlockTemplate(Some(opts))
      getBlockF
        .map(_ => succeed)
    }
  }

  it should "be able to generate blocks" in {
    for {
      (client, _) <- clientsF
      blocks <- client.generate(3)
    } yield assert(blocks.length == 3)
  }

  it should "be able to generate a block" in {
    for {
      (client, _) <- clientsF
      address <- client.getNewAddress
      unspent <- client.listUnspent
      changeAddress <- client.getRawChangeAddress
      rawTx <- {
        val output =
          unspent.find(output => output.amount.toBigDecimal > 1).get
        val input =
          TransactionInput(
            TransactionOutPoint(output.txid.flip, UInt32(output.vout)),
            ScriptSignature.empty,
            UInt32.max - UInt32(2))
        val inputs = Vector(input)

        val outputs =
          Map(address -> Bitcoins(0.5),
              changeAddress -> Bitcoins(output.amount.toBigDecimal - 0.55))

        client.createRawTransaction(inputs, outputs)
      }
      stx <- BitcoindRpcTestUtil.signRawTransaction(client, rawTx)
      address2 <- client.getNewAddress
      block <- client.generateBlock(address2, Vector(stx.hex))
      tx <- client.getRawTransaction(stx.hex.txIdBE, Some(block))
    } yield {
      assert(block != DoubleSha256DigestBE.empty)
      assert(tx.hex == stx.hex)
    }
  }

  it should "be able to get the mining info" in {
    for {
      (client, _) <- clientsF
      info <- client.getMiningInfo
    } yield assert(info.chain == "regtest")
  }

  it should "be able to generate blocks to an address" in {
    for {
      (client, otherClient) <- clientsF
      address <- otherClient.getNewAddress
      blocks <- client.generateToAddress(3, address)
      foundBlocks <- {
        val hashFuts = blocks.map(client.getBlockWithTransactions)
        Future.sequence(hashFuts)
      }
    } yield {
      assert(blocks.length == 3)
      assert(blocks.length == 3)
      foundBlocks.foreach { case found: GetBlockWithTransactionsResultV22 =>
        assert(
          found.tx.exists(
            _.vout.exists(_.scriptPubKey.address == Some(address))))
      }
      succeed
    }
  }

  it should "be able to generate blocks and then get their serialized headers" in {
    for {
      (client, _) <- clientsF
      blocks <- client.generate(2)
      header <- client.getBlockHeaderRaw(blocks(1))
    } yield assert(header.previousBlockHashBE == blocks(0))
  }

  it should "be able to generate blocks and then get their headers" in {
    for {
      (client, _) <- clientsF
      blocks <- client.generate(2)
      firstHeader <- client.getBlockHeader(blocks(0))
      secondHeader <- client.getBlockHeader(blocks(1))
    } yield {
      assert(firstHeader.nextblockhash.contains(blocks(1)))
      assert(secondHeader.previousblockhash.contains(blocks(0)))
      assert(secondHeader.nextblockhash.isEmpty)
    }
  }

  it should "be able to get the network hash per sec" in {
    for {
      (client, _) <- clientsF
      hps <- client.getNetworkHashPS()
    } yield assert(hps > 0)
  }
}
