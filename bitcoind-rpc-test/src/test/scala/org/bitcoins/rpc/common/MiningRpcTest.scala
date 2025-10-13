package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.{
  GetBlockWithTransactionsResultV22,
  RpcOpts
}
import org.bitcoins.core.api.chain.db.BlockHeaderDbHelper
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.blockchain.RegTestNetChainParams
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.chain.BlockHeaderHelper
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairNewest,
  BitcoindRpcTestUtil
}

import scala.concurrent.Future

class MiningRpcTest extends BitcoindFixturesCachedPairNewest {

  behavior of "MiningRpc"

  it should "be able to get a block template" in { case nodePair =>
    val client = nodePair.node1
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

  it should "be able to generate blocks" in { case nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.generate(3)
    } yield assert(blocks.length == 3)
  }

  it should "be able to generate a block" in { case nodePair =>
    val client = nodePair.node1
    for {
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
            UInt32.max - UInt32(2)
          )
        val inputs = Vector(input)

        val outputs =
          Map(
            address -> Bitcoins(0.5),
            changeAddress -> Bitcoins(output.amount.toBigDecimal - 0.55)
          )

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

  it should "be able to get the mining info" in { case nodePair =>
    val client = nodePair.node1
    for {
      info <- client.getMiningInfo
    } yield {
      assert(info.chain == "regtest")
    }
  }

  it should "be able to generate blocks to an address" in { case nodePair =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
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
            _.vout.exists(_.scriptPubKey.address == Some(address))
          )
        )
      }
      succeed
    }
  }

  it should "be able to generate blocks and then get their serialized headers" in {
    case nodePair =>
      val client = nodePair.node1
      for {
        blocks <- client.generate(2)
        header <- client.getBlockHeaderRaw(blocks(1))
      } yield assert(header.previousBlockHashBE == blocks(0))
  }

  it should "be able to generate blocks and then get their headers" in {
    case nodePair =>
      val client = nodePair.node1
      for {
        blocks <- client.generate(2)
        firstHeader <- client.getBlockHeader(blocks(0))
        secondHeader <- client.getBlockHeader(blocks(1))
      } yield {
        assert(firstHeader.nextblockhash.contains(blocks(1)))
        assert(secondHeader.previousblockhash.contains(blocks(0)))
        assert(secondHeader.nextblockhash.isEmpty)
      }
  }

  it should "be able to get the network hash per sec" in { case nodePair =>
    val client = nodePair.node1
    for {
      hps <- client.getNetworkHashPS()
    } yield assert(hps > 0)
  }

  it should "successfully submit a header" in { case nodePair =>
    val client = nodePair.node1
    val genesisHeader = RegTestNetChainParams.genesisBlock.blockHeader
    val genesisHeaderDb =
      BlockHeaderDbHelper.fromBlockHeader(height = 1, BigInt(0), genesisHeader)
    val nextHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
    client.submitHeader(nextHeader.blockHeader).map(_ => succeed)
  }
}
