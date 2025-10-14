package org.bitcoins.rpc.common

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{
  AddressType,
  ScanBlocksOpt
}
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.gcs.{BlockFilter, FilterType}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, ScriptSignature}
import org.bitcoins.core.protocol.script.descriptor.{
  AddressDescriptor,
  P2WPKHDescriptor
}
import org.bitcoins.core.protocol.transaction.{
  EmptyWitness,
  TransactionConstants,
  TransactionInput,
  TransactionOutPoint,
  TransactionOutput,
  WitnessTransaction
}
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairNewest,
  BitcoindRpcTestUtil
}
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class BlockchainRpcTest extends BitcoindFixturesCachedPairNewest {

  behavior of "BlockchainRpc"

  it should "be able to get the first block" in { nodePair =>
    val client = nodePair.node1
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
    } yield {
      assert(block.tx.nonEmpty)
      assert(block.height == 1)
    }
  }

  it should "be able to get blockchain info" in { nodePair =>
    val client = nodePair.node1
    for {
      info <- client.getBlockChainInfo
      bestHash <- client.getBestBlockHash()
    } yield {
      assert(info.isInstanceOf[GetBlockChainInfoResultPostV27])
      val postV27 = info.asInstanceOf[GetBlockChainInfoResultPostV27]
      assert(postV27.chain == RegTest)
      assert(postV27.bestblockhash == bestHash)
      assert(postV27.target.isDefined)
    }
  }

  it should "be able to invalidate a block" in { nodePair =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
      address <- otherClient.getNewAddress(addressType = AddressType.P2SHSegwit)
      txid <-
        BitcoindRpcTestUtil
          .fundMemPoolTransaction(client, address, Bitcoins(1))
      blocks <- client.generate(1)
      mostRecentBlock <- client.getBlock(blocks.head)
      _ <- client.invalidateBlock(blocks.head)
      mempool <- client.getRawMemPool().map(_.txids)
      count1 <- client.getBlockCount()
      count2 <- otherClient.getBlockCount()

      _ <- client.generate(
        2
      ) // Ensure client and otherClient have the same blockchain
    } yield {
      assert(mostRecentBlock.tx.contains(txid))
      assert(mempool.contains(txid))
      assert(count1 == count2 - 1)
    }
  }

  it should "be able to get block hash by height" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.generate(2)
      count <- client.getBlockCount()
      hash <- client.getBlockHash(count)
      prevhash <- client.getBlockHash(count - 1)
    } yield {
      assert(blocks(1) == hash)
      assert(blocks(0) == prevhash)
    }
  }

  it should "be able to get tx out proof and verify it" in { nodePair =>
    val client = nodePair.node1
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      merkle <- client.getTxOutProof(Vector(block.tx.head.txid))
      txids <- client.verifyTxOutProof(merkle)
    } yield {
      assert(merkle.transactionCount == UInt32(1))
      assert(merkle.hashes.length == 1)
      assert(merkle.hashes.head.flip == block.tx.head.txid)
      assert(block.tx.head.txid == txids.head)
    }
  }

  it should "be able to rescan the blockchain" in { nodePair =>
    val client = nodePair.node1
    for {
      result <- client.rescanBlockChain()
      count <- client.getBlockCount()
    } yield {
      assert(result.start_height == 0)
      assert(count == result.stop_height)
    }
  }

  it should "be able to get the chain tx stats" in { nodePair =>
    val client = nodePair.node1
    for {
      stats <- client.getChainTxStats
    } yield {
      assert(stats.txcount > 0)
      assert(stats.window_block_count > 0)
    }
  }

  it should "be able to get a raw block" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.generate(1)
      block <- client.getBlockRaw(blocks.head)
      blockHeader <- client.getBlockHeaderRaw(blocks.head)
    } yield assert(block.blockHeader == blockHeader)
  }

  it should "be able to get a block" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.generate(1)
      block <- client.getBlock(blocks.head)
    } yield {
      assert(block.hash == blocks(0))
      assert(block.confirmations == 1)
      assert(block.size > 0)
      assert(block.weight > 0)
      assert(block.height > 0)
      assert(block.difficulty > 0)
      assert(block.target.isDefined)
    }
  }

  it should "be able to get a transaction" in { nodePair =>
    val client = nodePair.node1
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      tx <- client.getTransaction(block.tx.head.txid)
      count <- client.getBlockCount()
    } yield {
      assert(tx.txid == block.tx.head.txid)
      assert(tx.amount == Bitcoins(50))
      assert(tx.blockindex.get == 0)
      assert(tx.details.head.category == "generate")
      assert(tx.generated.get)
      assert(tx.confirmations == count)
    }
  }

  it should "be able to get a block with verbose transactions" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.generate(2)
      block <- client.getBlockWithTransactions(blocks(1))
    } yield {
      assert(block.hash == blocks(1))
      assert(block.tx.length == 1)
      val tx = block.tx.head
      assert(tx.vout.head.n == 0)
    }
  }

  it should "be able to get the chain tips" in { nodePair =>
    val client = nodePair.node1
    for {
      _ <- client.getChainTips
    } yield succeed
  }

  it should "be able to get the best block hash" in { nodePair =>
    val client = nodePair.node1
    for {
      _ <- client.getBestBlockHash()
    } yield succeed
  }

  it should "be able to list all blocks since a given block" in { nodePair =>
    val client = nodePair.node1
    for {
      addr <- client.getNewAddress
      blocks <- client.generateToAddress(3, addr)
      list <- client.listSinceBlock(blocks(0))
    } yield {
      assert(list.transactions.length >= 2)
      assert(list.transactions.exists(_.blockhash.contains(blocks(1))))
      assert(list.transactions.exists(_.blockhash.contains(blocks(2))))
    }
  }

  it should "be able to verify the chain" in { nodePair =>
    val client = nodePair.node1
    for {
      valid <- client.verifyChain(blocks = 0)
    } yield assert(valid)
  }

  it should "be able to get the tx outset info" in { nodePair =>
    val client = nodePair.node1
    for {
      info <- client.getTxOutSetInfo
      count <- client.getBlockCount()
      hash <- client.getBestBlockHash()
    } yield {
      assert(info.height == count)
      assert(info.bestblock == hash)
    }
  }

  it should "calculate median time past" in { nodePair =>
    val client = nodePair.node1
    for {
      medianTime <- client.getMedianTimePast()
    } yield {
      val oneHourAgo = (System.currentTimeMillis() / 1000) - 60 * 60
      assert(medianTime > oneHourAgo)
    }
  }
  it should "get a block filter given a block hash" in { nodePair =>
    val client = nodePair.node1
    for {
      addr <- client.getNewAddress
      blocks <- client.generateToAddress(1, addr)
      blockFilter <- client.getBlockFilter(blocks.head, FilterType.Basic)

      block <- client.getBlockRaw(blocks.head)
      txs <- Future.sequence(
        block.transactions
          .filterNot(_.isCoinbase)
          .map(tx => client.getTransaction(tx.txIdBE))
      )

      prevFilter <- client.getBlockFilter(
        block.blockHeader.previousBlockHashBE,
        FilterType.Basic
      )
    } yield {
      val pubKeys = txs.flatMap(_.hex.outputs.map(_.scriptPubKey)).toVector
      val filter = BlockFilter(block, pubKeys)
      assert(filter.hash == blockFilter.filter.hash)
      assert(
        blockFilter.header == filter
          .getHeader(prevFilter.header.flip)
          .hash
          .flip
      )
    }
  }

  it should "start scanning blocks" in { case nodePair =>
    val client = nodePair.node1
    val privKey = ECPrivateKey.freshPrivateKey
    val np = RegTest
    val descriptor = P2WPKHDescriptor(privKey, np)
    // val spk = P2WPKHWitnessSPKV0(privKey.publicKey)
    // val importedAddress = Bech32Address.fromScriptPubKey(spk, np)
    val scanObjects = Vector(ScanObject(descriptor))
    val request0 = ScanBlocksRequest(action = ScanBlocksOpt.Status,
                                     scanObjects = Vector.empty,
                                     startHeightOpt = None,
                                     stopHeightOpt = None,
                                     filterTypeOpt = None)
    val request1 = ScanBlocksRequest(action = ScanBlocksOpt.Start,
                                     scanObjects = scanObjects,
                                     startHeightOpt = None,
                                     stopHeightOpt = None,
                                     filterTypeOpt = None)
    val abortedReq = ScanBlocksRequest(action = ScanBlocksOpt.Abort,
                                       Vector.empty,
                                       startHeightOpt = None,
                                       stopHeightOpt = None,
                                       filterTypeOpt = None)
    for {
      response0 <- client
        .scanBlocks(request0)
      response1 <- client.scanBlocks(request1)
      response2 <- client.scanBlocks(abortedReq)
    } yield {
      assert(response0 == NoScanInProgress)
      val start = response1.asInstanceOf[ScanBlocksStartResult]
      assert(start.from_height == 0)
      assert(start.relevant_blocks.isEmpty)
      assert(!response2.asInstanceOf[ScanBlocksAbortResult].aborted)
    }
  }

  it should "find an address a payment was made" in { case nodePair =>
    val client = nodePair.node1
    val privKey = ECPrivateKey.freshPrivateKey
    val p2pwkh = P2WPKHWitnessSPKV0(privKey.publicKey)
    val address = BitcoinAddress.fromScriptPubKey(p2pwkh, RegTest)
    val request0 =
      ScanBlocksRequest(action = ScanBlocksOpt.Start,
                        scanObjects =
                          Vector(AddressDescriptor(address)).map(ScanObject),
                        startHeightOpt = None,
                        stopHeightOpt = None,
                        filterTypeOpt = None)
    for {
      _ <- client.sendToAddress(address, Bitcoins.one)
      _ <- client.generate(6)
      response0 <- client.scanBlocks(request0)
      blockCount <- client.getBlockCount()
    } yield {
      val start = response0.asInstanceOf[ScanBlocksStartResult]
      assert(start.from_height == 0)
      assert(start.to_height == blockCount)
      assert(start.relevant_blocks.nonEmpty)
    }

  }

  it must "be able to getchainstates" in { case nodePair =>
    val client = nodePair.node1
    val bestBlockHashF = client.getBestBlockHash()
    val blockCountF = client.getBlockCount()
    for {
      bestBlockHash <- bestBlockHashF
      blockCount <- blockCountF
      chainStateResult <- client.getChainStates()
    } yield {
      assert(chainStateResult.headers == blockCount)
      assert(chainStateResult.chainstates.size == 1)
      assert(chainStateResult.chainstates.head.bestblockhash == bestBlockHash)
      assert(chainStateResult.chainstates.head.target.isDefined)
    }
  }

  it must "getdescriptoractivity" in { case nodePair =>
    val client = nodePair.node1
    val addressF = client.getNewAddress
    val fundTxidF = addressF.flatMap(client.sendToAddress(_, Bitcoins.one))
    val spendTxIdF = for {
      _ <- fundTxidF
      _ <- client.generate(1)
      address <- addressF
      unspent <- client.listUnspent.map { unspents =>
        unspents.filter(_.address.contains(address)).head
      }
      outpoint = TransactionOutPoint(unspent.txid, unspent.vout)
      addr2 <- client.getNewAddress
      unsignedSpendTx = WitnessTransaction(
        TransactionConstants.version,
        Vector(
          TransactionInput(outpoint,
                           ScriptSignature.empty,
                           TransactionConstants.sequence)),
        Vector(
          TransactionOutput(Bitcoins.one - Satoshis(500), addr2.scriptPubKey)),
        TransactionConstants.validLockVersionU32,
        EmptyWitness.fromN(1)
      )
      signedSpendTx <- client.signRawTransactionWithWallet(unsignedSpendTx)
      signedTxId <- client.sendRawTransaction(signedSpendTx.hex)
    } yield {
      signedTxId
    }

    for {
      fundTxId <- fundTxidF
      spendTxId <- spendTxIdF
      address <- addressF
      fundTxId2 <- client.sendToAddress(address, Bitcoins.two)
      addressDesc = AddressDescriptor.apply(address)
      blockHashes <- client.generate(1)
      activityResult <- client.getDescriptorActivity(blockHashes,
                                                     Vector(addressDesc))
    } yield {
      assert(activityResult.activity.nonEmpty)
      val spends = activityResult.activity.collect {
        case s: DescriptorActivity.SpendDescriptorActivity => s
      }
      val receives = activityResult.activity.collect {
        case r: DescriptorActivity.ReceiveDescriptorActivity => r
      }
      assert(spends.exists(s =>
        s.spend_txid == spendTxId && s.prevout_txid == fundTxId))
      assert(receives.exists(r => r.txid == fundTxId2))
    }
  }

  it must "waitfornewblock" in { case nodePair =>
    val client = nodePair.node1
    val bestHashF = client.getBestBlockHash()
    val timeout = 0.millis // no timeout
    for {
      bestHash <- bestHashF
      waitF = client.waitForNewBlock(timeout, Some(bestHash))
      _ <- AsyncUtil.nonBlockingSleep(
        1.second
      ) // wait a bit to make sure future isn't complete
      _ = assert(!waitF.isCompleted)
      hashes <- client.generate(1)
      wait <- waitF
    } yield {
      assert(hashes.head == wait.hash)
    }
  }

  it must "waitforblock" in { case nodePair =>
    val client = nodePair.node1
    val timeout = 0.millis // no timeout
    for {
      hashes <- client.generate(1)
      wait <- client.waitForBlock(timeout, hashes.head)
    } yield {
      assert(hashes.head == wait.hash)
    }
  }

  it must "waitforblockheight" in { case nodePair =>
    val client = nodePair.node1
    val heightF = client.getBlockCount()
    val timeout = 0.millis // no timeout
    for {
      height <- heightF
      waitF = client.waitForBlockHeight(timeout, height + 1)
      _ <- AsyncUtil.nonBlockingSleep(
        1.second
      ) // wait a bit to make sure future isn't complete
      _ = assert(!waitF.isCompleted)
      hashes <- client.generate(1)
      wait <- waitF
    } yield {
      assert(hashes.head == wait.hash)
      assert(height + 1 == wait.height)
    }
  }
}
