package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  EmptyScriptSignature,
  ScriptPubKey,
  ScriptSignature
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.commons.rpc.BitcoindException.InvalidAddressOrKey
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairNewest,
  BitcoindRpcTestUtil
}

class RawTransactionRpcTest extends BitcoindFixturesCachedPairNewest {

  behavior of "RawTransactionRpc"

  it should "be able to fund a raw transaction" in { case nodePair =>
    val (client, otherClient) = (nodePair.node1, nodePair.node2)
    for {
      address <- otherClient.getNewAddress
      transactionWithoutFunds <-
        client
          .createRawTransaction(Vector.empty, Map(address -> Bitcoins(1)))
      transactionResult <- client.fundRawTransaction(transactionWithoutFunds)
      transaction = transactionResult.hex
      inputTransaction <-
        client
          .getRawTransaction(transaction.inputs.head.previousOutput.txId.flip)
    } yield {
      assert(transaction.inputs.length == 1)

      val inputTxSats =
        inputTransaction
          .vout(transaction.inputs.head.previousOutput.vout.toInt)
          .value

      val txResultSats =
        transactionResult.fee +
          transaction.outputs.head.value +
          transaction.outputs(1).value

      assert(txResultSats == inputTxSats)
    }
  }

  it should "be able to decode a raw transaction" in { case nodePair =>
    val (client, otherClient) = (nodePair.node1, nodePair.node2)
    for {
      transaction <-
        BitcoindRpcTestUtil
          .createRawCoinbaseTransaction(client, otherClient)
      rpcTransaction <- client.decodeRawTransaction(transaction)
    } yield {
      assert(rpcTransaction.txid == transaction.txIdBE)
      assert(rpcTransaction.locktime == transaction.lockTime)
      assert(rpcTransaction.size == transaction.byteSize)
      assert(rpcTransaction.version == transaction.version.toInt)
      assert(rpcTransaction.vsize == transaction.vsize)
    }
  }

  it should "be able to get a raw transaction using both rpcs available" in {
    case nodePair =>
      val client = nodePair.node1
      for {
        block <- BitcoindRpcTestUtil.getFirstBlock(client)
        txid = block.tx.head.txid
        transaction1 <- client.getRawTransaction(txid)
        transaction2 <- client.getTransaction(txid)
      } yield {
        assert(transaction1.txid == transaction2.txid)
        assert(transaction1.confirmations.contains(transaction2.confirmations))
        assert(transaction1.hex == transaction2.hex)

        assert(transaction1.blockhash.isDefined)
        assert(transaction2.blockhash.isDefined)

        assert(transaction1.blockhash == transaction2.blockhash)
      }
  }

  it should "be able to create a raw transaction" in { case nodePair =>
    val (client, otherClient) = (nodePair.node1, nodePair.node2)
    for {
      blocks <- client.generate(2)
      firstBlock <- client.getBlock(blocks(0))
      transaction0 <- client.getTransaction(firstBlock.tx(0))
      secondBlock <- client.getBlock(blocks(1))
      transaction1 <- client.getTransaction(secondBlock.tx(0))

      address <- otherClient.getNewAddress

      input0 = TransactionOutPoint(
        transaction0.txid.flip,
        UInt32(transaction0.blockindex.get)
      )
      input1 = TransactionOutPoint(
        transaction1.txid.flip,
        UInt32(transaction1.blockindex.get)
      )
      transaction <- {
        val sig: ScriptSignature = ScriptSignature.empty
        val inputs = Vector(
          TransactionInput(input0, sig, UInt32(1)),
          TransactionInput(input1, sig, UInt32(2))
        )
        val outputs = Map(address -> Bitcoins(1))
        client.createRawTransaction(inputs, outputs)
      }
    } yield {
      val inputs = transaction.inputs
      assert(inputs.head.sequence == UInt32(1))
      assert(inputs(1).sequence == UInt32(2))
      assert(inputs.head.previousOutput.txId == input0.txId)
      assert(inputs(1).previousOutput.txId == input1.txId)
    }
  }

  it should "be able to send a raw transaction to the mem pool" in {
    case nodePair =>
      val (client, otherClient) = (nodePair.node1, nodePair.node2)
      for {
        rawTx <-
          BitcoindRpcTestUtil.createRawCoinbaseTransaction(client, otherClient)
        signedTransaction <- BitcoindRpcTestUtil.signRawTransaction(client,
                                                                    rawTx)

        _ <- client.generate(101) // Can't spend coinbase until depth 100

        _ <- client.sendRawTransaction(signedTransaction.hex, maxfeerate = 0)
      } yield succeed
  }

  it should "be able to sign a raw transaction" in { case nodePair =>
    val (client, otherClient) = (nodePair.node1, nodePair.node2)
    val fundAmt = Bitcoins(1.2)
    val sendAmt = fundAmt.satoshis - Satoshis(1000)
    for {
      address <- client.getNewAddress
      txid <-
        BitcoindRpcTestUtil
          .fundBlockChainTransaction(client, otherClient, address, fundAmt)
      rawTx <- client.getTransaction(txid)
      tx <- client.decodeRawTransaction(rawTx.hex)
      output =
        tx.vout
          .find(output => output.value == fundAmt)
          .get

      newAddress <- client.getNewAddress
      rawCreatedTx <- {
        val input =
          TransactionInput(
            TransactionOutPoint(txid.flip, UInt32(output.n)),
            EmptyScriptSignature,
            UInt32.max - UInt32.one
          )
        client
          .createRawTransaction(
            Vector(input),
            Map(newAddress -> Bitcoins(sendAmt.satoshis))
          )
      }

      result <- {
        val utxoDeps = Vector(
          RpcOpts.SignRawTransactionOutputParameter(
            txid = txid,
            vout = output.n,
            scriptPubKey = ScriptPubKey.fromAsmHex(output.scriptPubKey.hex),
            redeemScript = None,
            amount = Some(fundAmt)
          )
        )
        BitcoindRpcTestUtil.signRawTransaction(
          client,
          rawCreatedTx,
          utxoDeps
        )
      }
    } yield assert(result.complete)
  }

  it should "fail to abandon a transaction which has not been sent" in {
    case nodePair =>
      val (client, otherClient) = (nodePair.node1, nodePair.node2)
      otherClient.getNewAddress.flatMap { address =>
        client
          .createRawTransaction(Vector(), Map(address -> Bitcoins(1)))
          .flatMap { tx =>
            recoverToSucceededIf[InvalidAddressOrKey](
              client.abandonTransaction(tx.txId)
            )
          }
      }
  }

  it should "be able to get a raw transaction in serialized form from the mem pool" in {
    case nodePair =>
      val (client, otherClient) = (nodePair.node1, nodePair.node2)
      for {
        sentTx <- BitcoindRpcTestUtil.sendCoinbaseTransaction(client,
                                                              otherClient)
        rawTx <- client.getRawTransactionRaw(sentTx.txid)
      } yield assert(rawTx.txIdBE == sentTx.txid)
  }

  it should "output more than one txid" in { case nodePair =>
    val (client, otherClient) = (nodePair.node1, nodePair.node2)
    for {
      blocks <- client.generate(2)
      firstBlock <- client.getBlock(blocks(0))
      transaction0 <- client.getTransaction(firstBlock.tx(0))
      secondBlock <- client.getBlock(blocks(1))
      transaction1 <- client.getTransaction(secondBlock.tx(0))

      address <- otherClient.getNewAddress

      input0 = TransactionOutPoint(
        transaction0.txid.flip,
        UInt32(transaction0.blockindex.get)
      )
      input1 = TransactionOutPoint(
        transaction1.txid.flip,
        UInt32(transaction1.blockindex.get)
      )

      transactionFirst <- {
        val sig: ScriptSignature = ScriptSignature.empty
        val inputs = Vector(
          TransactionInput(input0, sig, UInt32(1)),
          TransactionInput(input1, sig, UInt32(2))
        )
        val outputs = Map(address -> Bitcoins(1))
        client.createRawTransaction(inputs, outputs)
      }
      fundedTransactionOne <- client.fundRawTransaction(transactionFirst)
      signedTransactionOne <- BitcoindRpcTestUtil.signRawTransaction(
        client,
        fundedTransactionOne.hex
      )

      blocksTwo <- client.generate(2)
      firstBlockTwo <- client.getBlock(blocksTwo(0))
      transaction2 <- client.getTransaction(firstBlockTwo.tx(0))
      secondBlockTwo <- client.getBlock(blocksTwo(1))
      transaction3 <- client.getTransaction(secondBlockTwo.tx(0))

      input2 = TransactionOutPoint(
        transaction2.txid.flip,
        UInt32(transaction2.blockindex.get)
      )
      input3 = TransactionOutPoint(
        transaction3.txid.flip,
        UInt32(transaction3.blockindex.get)
      )

      transactionSecond <- {
        val sig: ScriptSignature = ScriptSignature.empty
        val inputs = Vector(
          TransactionInput(input2, sig, UInt32(1)),
          TransactionInput(input3, sig, UInt32(2))
        )
        val outputs = Map(address -> Bitcoins(1))
        client.createRawTransaction(inputs, outputs)
      }
      fundedTransactionTwo <- client.fundRawTransaction(transactionSecond)
      signedTransactionTwo <- BitcoindRpcTestUtil.signRawTransaction(
        client,
        fundedTransactionTwo.hex
      )
      _ <- client.generate(100) // Can't spend until depth 100
      mempoolAccept <- client.testMempoolAccept(
        Vector(signedTransactionOne.hex, signedTransactionTwo.hex)
      )
    } yield {
      val mempooltxid: Int = mempoolAccept.length
      assert(mempooltxid > 1)
      val entry = mempoolAccept.head
      assert(entry.fees.get.base == SatoshisPerByte(Satoshis(4)))
      assert(entry.fees.get.effective_feerate == BigDecimal(0.0002))
      assert(entry.fees.get.effective_includes.nonEmpty)
    }
  }
}
