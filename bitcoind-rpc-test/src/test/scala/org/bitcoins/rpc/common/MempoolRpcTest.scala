package org.bitcoins.rpc.common

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.rpc.BitcoindException
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairNewest,
  BitcoindRpcTestUtil
}
import org.scalatest.{FutureOutcome, Outcome}

import java.io.File
import scala.concurrent.Future

class MempoolRpcTest extends BitcoindFixturesCachedPairNewest {

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val futOutcome: Future[Outcome] = for {
      nodePair <- clientsF
      futOutcome = with2BitcoindsCached(test, nodePair)
      f <- futOutcome.toFuture
    } yield f
    new FutureOutcome(futOutcome)
  }

  behavior of "MempoolRpc"

  it should "be able to find a transaction sent to the mem pool" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val otherClient = nodePair.node2
      for {
        transaction <-
          BitcoindRpcTestUtil.sendCoinbaseTransaction(client, otherClient)
        mempool <- client.getRawMemPool
      } yield {
        assert(mempool.length == 1)
        assert(mempool.head == transaction.txid)
      }
  }

  it should "be able to find a verbose transaction in the mem pool" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val otherClient = nodePair.node2
      for {
        transaction <-
          BitcoindRpcTestUtil.sendCoinbaseTransaction(client, otherClient)
        mempool <- client.getRawMemPoolWithTransactions
      } yield {
        val txid = mempool.keySet.head
        assert(txid == transaction.txid)
        assert(mempool(txid).size > 0)
      }
  }

  it should "be able to find a mem pool entry" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
      transaction <-
        BitcoindRpcTestUtil.sendCoinbaseTransaction(client, otherClient)
      _ <- client.getMemPoolEntry(transaction.txid)
    } yield succeed
  }

  it must "fail to find a mempool entry" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val txid = DoubleSha256Digest.empty
    val resultF = for {

      result <- client.getMemPoolEntry(txid)
    } yield {
      result
    }

    recoverToSucceededIf[BitcoindException](resultF)
  }

  it must "fail to find a mempool entry and return None" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val txid = DoubleSha256Digest.empty
      val resultF = for {
        result <- client.getMemPoolEntryOpt(txid)
      } yield {
        assert(result.isEmpty)
      }
      resultF
  }
  it should "be able to get mem pool info" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
      _ <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
      info <- client.getMemPoolInfo
      _ <-
        BitcoindRpcTestUtil
          .sendCoinbaseTransaction(client, otherClient)
      newInfo <- client.getMemPoolInfo
    } yield {
      assert(info.size == 0)
      assert(newInfo.size == 1)
    }
  }

  it should "be able to prioritise a mem pool transaction" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val otherClient = nodePair.node2
      for {
        address <- otherClient.getNewAddress
        txid <-
          BitcoindRpcTestUtil
            .fundMemPoolTransaction(client, address, Bitcoins(3.2))
        entry <- client.getMemPoolEntry(txid)
        tt <- client.prioritiseTransaction(txid, Bitcoins(1).satoshis)
        newEntry <- client.getMemPoolEntry(txid)
      } yield {
        assert(entry.fee == entry.modifiedfee)
        assert(tt)
        assert(newEntry.fee == entry.fee)
        assert(newEntry.modifiedfee == newEntry.fee + Bitcoins(1))
      }
  }

  it should "be able to find mem pool ancestors and descendants" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      for {
        _ <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
        address1 <- client.getNewAddress
        txid1 <- BitcoindRpcTestUtil.fundMemPoolTransaction(client,
                                                            address1,
                                                            Bitcoins(2))
        mempool <- client.getRawMemPool
        address2 <- client.getNewAddress

        createdTx <- {
          val input: TransactionInput =
            TransactionInput(TransactionOutPoint(txid1.flip, UInt32.zero),
                             ScriptSignature.empty,
                             UInt32.max - UInt32.one)
          client
            .createRawTransaction(Vector(input), Map(address2 -> Bitcoins.one))
        }
        signedTx <- BitcoindRpcTestUtil.signRawTransaction(client, createdTx)
        txid2 <- client.sendRawTransaction(signedTx.hex, maxfeerate = 0)

        descendantsTxid1 <- client.getMemPoolDescendants(txid1)
        verboseDescendantsTxid1 <- client.getMemPoolDescendantsVerbose(txid1)
        _ = {
          assert(descendantsTxid1.head == txid2)
          val (txid, mempoolresults) = verboseDescendantsTxid1.head
          assert(txid == txid2)
          assert(mempoolresults.ancestorcount == 2)
        }

        ancestorsTxid2 <- client.getMemPoolAncestors(txid2)
        verboseAncestorsTxid2 <- client.getMemPoolAncestorsVerbose(txid2)
        _ = {
          assert(ancestorsTxid2.head == txid1)
          val (txid, mempoolresults) = verboseAncestorsTxid2.head
          assert(txid == txid1)
          assert(mempoolresults.descendantcount == 2)
        }

      } yield {
        assert(mempool.head == txid1)
        assert(signedTx.complete)
      }
  }

  it should "be able to save the mem pool to disk" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val regTest =
        new File(client.getDaemon.datadir.getAbsolutePath + "/regtest")
      assert(regTest.isDirectory)
      assert(!regTest.list().contains("mempool.dat"))
      for {
        _ <- client.saveMemPool()
      } yield assert(regTest.list().contains("mempool.dat"))
  }
}
