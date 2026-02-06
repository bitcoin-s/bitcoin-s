package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.SignRawTransactionOutputParameter
import org.bitcoins.commons.jsonmodels.bitcoind.{
  GetMemPoolInfoResultV29,
  GetMemPoolInfoResultV30
}
import org.bitcoins.commons.rpc.BitcoindException
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.rpc.config.{BitcoindInstanceLocal, BitcoindInstanceRemote}
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
    nodePair =>
      val client = nodePair.node1
      val otherClient = nodePair.node2
      for {
        transaction <-
          BitcoindRpcTestUtil.sendCoinbaseTransaction(client, otherClient)
        mempool <- client.getRawMemPool().map(_.txids)
      } yield {
        assert(mempool.length == 1)
        assert(mempool.head == transaction.txid)
      }
  }

  it should "be able to find a verbose transaction in the mem pool" in {
    nodePair =>
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

  it should "be able to find a mem pool entry" in { nodePair =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
      transaction <-
        BitcoindRpcTestUtil.sendCoinbaseTransaction(client, otherClient)
      _ <- client.getMemPoolEntry(transaction.txid)
    } yield succeed
  }

  it must "fail to find a mempool entry" in { nodePair =>
    val client = nodePair.node1
    val txid = DoubleSha256Digest.empty
    val resultF = for {

      result <- client.getMemPoolEntry(txid)
    } yield {
      result
    }

    recoverToSucceededIf[BitcoindException](resultF)
  }

  it must "fail to find a mempool entry and return None" in { nodePair =>
    val client = nodePair.node1
    val txid = DoubleSha256Digest.empty
    val resultF = for {
      result <- client.getMemPoolEntryOpt(txid)
    } yield {
      assert(result.isEmpty)
    }
    resultF
  }
  it should "be able to get mem pool info" in { nodePair =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
      _ <- client.generate(1)
      info <- client.getMemPoolInfo
      _ <-
        BitcoindRpcTestUtil
          .sendCoinbaseTransaction(client, otherClient)
      newInfo <- client.getMemPoolInfo
    } yield {
      val defaultRelayFee = Bitcoins(Satoshis(100))
      assert(info.loaded)
      assert(info.size == 0)
      assert(info.fullrbf)
      assert(info.minrelaytxfee == defaultRelayFee)
      assert(info.incrementalrelayfee == defaultRelayFee.toBigDecimal)
      assert(info.unbroadcastcount == 0)
      assert(newInfo.size == 1)

      newInfo match {
        case v30: GetMemPoolInfoResultV30 =>
          assert(v30.permitbaremultisig)
          assert(v30.maxdatacarriersize == 100000)
        case _: GetMemPoolInfoResultV29 => fail()
      }

    }
  }

  it should "be able to prioritise a mem pool transaction" in { nodePair =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
      address <- otherClient.getNewAddress
      txid <-
        BitcoindRpcTestUtil
          .fundMemPoolTransaction(client, address, Bitcoins(3.2))
      tt <- client.prioritiseTransaction(txid, Bitcoins.one.satoshis)
      txs <- client.getPrioritisedTransactions()
    } yield {
      assert(tt)
      assert(txs.exists(_._1 == txid))
      val p = txs(txid)
      assert(p.in_mempool)
      assert(p.fee_delta == Bitcoins.one)
    }
  }

  it should "be able to find mem pool ancestors and descendants" in {
    nodePair =>
      val client = nodePair.node1
      for {
        _ <- client.generate(1)
        address1 <- client.getNewAddress
        txid1 <- BitcoindRpcTestUtil.fundMemPoolTransaction(
          client,
          address1,
          Bitcoins(2)
        )
        mempool <- client.getRawMemPool().map(_.txids)
        address2 <- client.getNewAddress

        createdTx <- {
          val input: TransactionInput =
            TransactionInput(
              TransactionOutPoint(txid1.flip, UInt32.zero),
              ScriptSignature.empty,
              UInt32.max - UInt32.one
            )
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

  it should "be able to save the mem pool to disk" in { nodePair =>
    val client = nodePair.node1
    val localInstance = client.getDaemon match {
      case _: BitcoindInstanceRemote =>
        sys.error(s"Cannot have remote bitcoind instance in tests")
      case local: BitcoindInstanceLocal => local
    }
    val regTest =
      new File(localInstance.datadir.getAbsolutePath + "/regtest")
    assert(regTest.isDirectory)
    assert(!regTest.list().contains("mempool.dat"))
    for {
      _ <- client.saveMemPool()
      mempoolPath = regTest.toPath.resolve("mempool.dat")
      _ <- client.importMempool(mempoolPath)
    } yield assert(regTest.list().contains("mempool.dat"))
  }

  it should "get tx spending prev out" in { nodePair =>
    val client = nodePair.node1
    val junkAddress: BitcoinAddress =
      BitcoinAddress("2NFyxovf6MyxfHqtVjstGzs6HeLqv92Nq4U")
    for {
      txid <- client.sendToAddress(junkAddress, Bitcoins.one)
      tx <- client.getRawTransaction(txid).map(_.hex)
      spending <- client.getTxSpendingPrevOut(tx.inputs.head.previousOutput)
    } yield assert(spending.spendingtxid.contains(txid))
  }

  it must "getrawmempool verbose" in { nodePair =>
    val client = nodePair.node1
    for {
      // generate block to clear out mempool for test
      _ <- client.generate(1)
      verbose0 <- client.getRawMempoolVerbose()
      addr0 <- client.getNewAddress
      txid <- client.sendToAddress(addr0, Bitcoins.one)
      verbose1 <- client.getRawMempoolVerbose()
    } yield {
      assert(verbose0.txids.isEmpty)
      assert(verbose1.txids.exists(_ == txid))
    }

  }

  it should "submit a package of transactions" in { nodePair =>
    val client = nodePair.node1
    for {
      // Create a parent transaction. We use fundRawTransaction to automatically
      // select inputs and add a change output, creating a valid transaction.
      address1 <- client.getNewAddress
      parentAmt = Bitcoins(1)
      transactionOne <- {
        val inputs = Vector.empty
        val outputs = Map(address1 -> parentAmt)
        client.createRawTransaction(inputs, outputs)
      }
      fundedTransactionOne <- client.fundRawTransaction(transactionOne)
      outputIdx = fundedTransactionOne.hex.outputs.zipWithIndex
        .find(_._1.scriptPubKey == address1.scriptPubKey)
        .map(_._2)
        .get
      signedParentTx <- BitcoindRpcTestUtil.signRawTransaction(
        client,
        fundedTransactionOne.hex
      )
      // Create a child transaction spending from the first output of parent.
      // This demonstrates the package CPFP (Child Pays For Parent) use case.
      address2 <- client.getNewAddress
      fee = Satoshis(20)
      transactionTwo <- {
        val sig: ScriptSignature = ScriptSignature.empty
        // Spend from first output (index 0) of parent transaction
        val input = TransactionInput(
          TransactionOutPoint(signedParentTx.hex.txIdBE, UInt32(outputIdx)),
          sig,
          sequenceNumber = UInt32.max - UInt32.one
        )
        // Create a transaction that spends most of the parent's output
        val amt = Bitcoins(parentAmt - fee)
        val outputs = Map(address2 -> amt)
        client.createRawTransaction(Vector(input), outputs)
      }
      // Sign the child transaction
      param = SignRawTransactionOutputParameter(
        txid = signedParentTx.hex.txIdBE,
        vout = outputIdx,
        scriptPubKey = address1.scriptPubKey,
        amount = Some(parentAmt)
      )
      signedChildTx <- BitcoindRpcTestUtil.signRawTransaction(
        client,
        transactionTwo,
        Vector(param)
      )
      // Submit as package - both parent and child together
      result <- client.submitPackage(
        Vector(signedParentTx.hex, signedChildTx.hex)
      )
      // Verify results
      mempool <- client.getRawMemPool().map(_.txids)
    } yield {
      // Check that package submission succeeded
      assert(result.package_msg == "success")
      assert(result.tx_results.size == 2)
      assert(result.tx_results.exists(_._2.fees.get.base == Bitcoins(fee)))
      // Both transactions should be in mempool
      assert(mempool.contains(signedParentTx.hex.txIdBE))
      assert(mempool.contains(signedChildTx.hex.txIdBE))
    }
  }
}
