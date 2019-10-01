package org.bitcoins.rpc.common

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  P2SHScriptSignature,
  ScriptPubKey,
  ScriptSignature
}
import org.bitcoins.core.protocol.transaction.{
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, RpcOpts}
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.concurrent.Future
import org.bitcoins.rpc.BitcoindException.InvalidAddressOrKey

class RawTransactionRpcTest extends BitcoindRpcTest {
  lazy val clientsF: Future[(BitcoindRpcClient, BitcoindRpcClient)] =
    BitcoindRpcTestUtil.createNodePairV17(clientAccum = clientAccum)

  behavior of "RawTransactionRpc"

  it should "be able to fund a raw transaction" in {
    for {
      (client, otherClient) <- clientsF
      address <- otherClient.getNewAddress
      transactionWithoutFunds <- client
        .createRawTransaction(Vector.empty, Map(address -> Bitcoins(1)))
      transactionResult <- client.fundRawTransaction(transactionWithoutFunds)
      transaction = transactionResult.hex
      inputTransaction <- client
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

  it should "be able to decode a raw transaction" in {
    for {
      (client, otherClient) <- clientsF
      transaction <- BitcoindRpcTestUtil
        .createRawCoinbaseTransaction(client, otherClient)
      rpcTransaction <- client.decodeRawTransaction(transaction)
    } yield {
      assert(rpcTransaction.txid == transaction.txIdBE)
      assert(rpcTransaction.locktime == transaction.lockTime)
      assert(rpcTransaction.size == transaction.size)
      assert(rpcTransaction.version == transaction.version.toInt)
      assert(rpcTransaction.vsize == transaction.vsize)
    }
  }

  it should "be able to get a raw transaction using both rpcs available" in {
    for {
      (client, _) <- clientsF
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

  it should "be able to create a raw transaction" in {
    for {
      (client, otherClient) <- clientsF
      blocks <- client.getNewAddress.flatMap(client.generateToAddress(2, _))
      firstBlock <- client.getBlock(blocks(0))
      transaction0 <- client.getTransaction(firstBlock.tx(0))
      secondBlock <- client.getBlock(blocks(1))
      transaction1 <- client.getTransaction(secondBlock.tx(0))

      address <- otherClient.getNewAddress

      input0 = TransactionOutPoint(transaction0.txid.flip,
                                   UInt32(transaction0.blockindex.get))
      input1 = TransactionOutPoint(transaction1.txid.flip,
                                   UInt32(transaction1.blockindex.get))
      transaction <- {
        val sig: ScriptSignature = ScriptSignature.empty
        val inputs = Vector(TransactionInput(input0, sig, UInt32(1)),
                            TransactionInput(input1, sig, UInt32(2)))
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
    for {
      (client, otherClient) <- clientsF
      rawTx <- BitcoindRpcTestUtil.createRawCoinbaseTransaction(client,
                                                                otherClient)
      signedTransaction <- BitcoindRpcTestUtil.signRawTransaction(client, rawTx)

      _ <- client.getNewAddress.flatMap(client.generateToAddress(100, _)) // Can't spend coinbase until depth 100

      _ <- client.sendRawTransaction(signedTransaction.hex,
                                     allowHighFees = true)
    } yield succeed
  }

  it should "be able to sign a raw transaction" in {
    for {
      (client, server) <- clientsF
      address <- client.getNewAddress
      pubkey <- BitcoindRpcTestUtil.getPubkey(client, address)
      multisig <- client
        .addMultiSigAddress(1, Vector(Left(pubkey.get)))
      txid <- BitcoindRpcTestUtil
        .fundBlockChainTransaction(client,
                                   server,
                                   multisig.address,
                                   Bitcoins(1.2))
      rawTx <- client.getTransaction(txid)

      tx <- client.decodeRawTransaction(rawTx.hex)
      output = tx.vout
        .find(output => output.value == Bitcoins(1.2))
        .get

      newAddress <- client.getNewAddress
      rawCreatedTx <- {
        val input =
          TransactionInput(TransactionOutPoint(txid.flip, UInt32(output.n)),
                           P2SHScriptSignature(multisig.redeemScript.hex),
                           UInt32.max - UInt32.one)
        client
          .createRawTransaction(Vector(input), Map(newAddress -> Bitcoins(1.1)))
      }

      result <- {
        val utxoDeps = Vector(
          RpcOpts.SignRawTransactionOutputParameter(
            txid,
            output.n,
            ScriptPubKey.fromAsmHex(output.scriptPubKey.hex),
            Some(multisig.redeemScript),
            amount = Some(Bitcoins(1.2))))
        BitcoindRpcTestUtil.signRawTransaction(
          client,
          rawCreatedTx,
          utxoDeps
        )
      }
    } yield assert(result.complete)
  }

  it should "be able to combine raw transactions" in {
    for {
      (client, otherClient) <- clientsF
      address1 <- client.getNewAddress
      address2 <- otherClient.getNewAddress
      pub1 <- BitcoindRpcTestUtil.getPubkey(client, address1)
      pub2 <- BitcoindRpcTestUtil.getPubkey(otherClient, address2)
      keys = Vector(Left(pub1.get), Left(pub2.get))

      multisig <- client.addMultiSigAddress(2, keys)

      _ <- otherClient.addMultiSigAddress(2, keys)

      txid <- BitcoindRpcTestUtil.fundBlockChainTransaction(client,
                                                            otherClient,
                                                            multisig.address,
                                                            Bitcoins(1.2))

      rawTx <- client.getTransaction(txid)
      tx <- client.decodeRawTransaction(rawTx.hex)

      output = tx.vout
        .find(output => output.value == Bitcoins(1.2))
        .get

      address3 <- client.getNewAddress

      ctx <- {
        val input =
          TransactionInput(TransactionOutPoint(txid.flip, UInt32(output.n)),
                           P2SHScriptSignature(multisig.redeemScript.hex),
                           UInt32.max - UInt32.one)
        otherClient
          .createRawTransaction(Vector(input), Map(address3 -> Bitcoins(1.1)))
      }

      txOpts = {
        val scriptPubKey =
          ScriptPubKey.fromAsmHex(output.scriptPubKey.hex)
        val utxoDep =
          RpcOpts.SignRawTransactionOutputParameter(
            txid,
            output.n,
            scriptPubKey,
            Some(multisig.redeemScript),
            amount = Some(Bitcoins(1.2)))
        Vector(utxoDep)
      }

      partialTx1 <- BitcoindRpcTestUtil.signRawTransaction(client, ctx, txOpts)

      partialTx2 <- BitcoindRpcTestUtil.signRawTransaction(otherClient,
                                                           ctx,
                                                           txOpts)

      combinedTx <- {
        val txs = Vector(partialTx1.hex, partialTx2.hex)
        client.combineRawTransaction(txs)
      }

      _ <- client.sendRawTransaction(combinedTx)

    } yield {
      assert(!partialTx1.complete)
      assert(partialTx1.hex != ctx)
      assert(!partialTx2.complete)
      assert(partialTx2.hex != ctx)
    }

  }

  it should "fail to abandon a transaction which has not been sent" in {
    clientsF.flatMap {
      case (client, otherClient) =>
        otherClient.getNewAddress.flatMap { address =>
          client
            .createRawTransaction(Vector(), Map(address -> Bitcoins(1)))
            .flatMap { tx =>
              recoverToSucceededIf[InvalidAddressOrKey](
                client.abandonTransaction(tx.txId))
            }
        }
    }
  }

  it should "be able to get a raw transaction in serialized form from the mem pool" in {
    for {
      (client, otherClient) <- clientsF

      sentTx <- BitcoindRpcTestUtil.sendCoinbaseTransaction(client, otherClient)
      rawTx <- client.getRawTransactionRaw(sentTx.txid)
    } yield assert(rawTx.txIdBE == sentTx.txid)
  }
}
