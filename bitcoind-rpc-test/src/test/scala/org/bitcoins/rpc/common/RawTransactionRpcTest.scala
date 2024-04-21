package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.commons.jsonmodels.bitcoind.{DecodeScriptResultV22, RpcOpts}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.protocol.script.{
  EmptyScriptSignature,
  ScriptPubKey,
  ScriptSignature
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.rpc.BitcoindException.InvalidAddressOrKey
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.concurrent.Future

class RawTransactionRpcTest extends BitcoindRpcTest {

  lazy val clientsF: Future[(BitcoindRpcClient, BitcoindRpcClient)] =
    BitcoindRpcTestUtil.createNodePair(clientAccum = clientAccum)

  behavior of "RawTransactionRpc"

  it should "be able to fund a raw transaction" in {
    for {
      (client, otherClient) <- clientsF
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

  it should "be able to decode a raw transaction" in {
    for {
      (client, otherClient) <- clientsF
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
    for {
      (client, otherClient) <- clientsF
      rawTx <-
        BitcoindRpcTestUtil.createRawCoinbaseTransaction(client, otherClient)
      signedTransaction <- BitcoindRpcTestUtil.signRawTransaction(client, rawTx)

      _ <- client.generate(101) // Can't spend coinbase until depth 100

      _ <- client.sendRawTransaction(signedTransaction.hex, maxfeerate = 0)
    } yield succeed
  }

  it should "be able to sign a raw transaction" in {
    val fundAmt = Bitcoins(1.2)
    val sendAmt = fundAmt.satoshis - Satoshis(1000)
    for {
      (client, server) <- clientsF
      address <- client.getNewAddress
      txid <-
        BitcoindRpcTestUtil
          .fundBlockChainTransaction(client, server, address, fundAmt)
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
    clientsF.flatMap { case (client, otherClient) =>
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
  }

  it should "be able to get a raw transaction in serialized form from the mem pool" in {
    for {
      (client, otherClient) <- clientsF

      sentTx <- BitcoindRpcTestUtil.sendCoinbaseTransaction(client, otherClient)
      rawTx <- client.getRawTransactionRaw(sentTx.txid)
    } yield assert(rawTx.txIdBE == sentTx.txid)
  }

  it should "be able to decode a reedem script" in {
    for {
      (client, _) <- clientsF
      ecPrivKey1 = ECPrivateKey.freshPrivateKey
      pubKey1 = ecPrivKey1.publicKey
      _ <- client.unloadWallet("")
      _ <- client.createWallet("decodeRWallet")
      address <- client.getNewAddress(addressType = AddressType.Legacy)
      multisig <- client.addMultiSigAddress(
        2,
        Vector(Left(pubKey1), Right(address.asInstanceOf[P2PKHAddress])))
      decoded <- client.decodeScript(multisig.redeemScript)
      _ <- client.loadWallet("")
      _ <- client.unloadWallet("decodeRWallet")
    } yield {
      decoded match {
        case decodedV22: DecodeScriptResultV22 =>
          assert(decodedV22.typeOfScript.contains(ScriptType.MULTISIG))
      }
    }
  }

  it should "output more than one txid" in {
    for {
      (client, otherClient) <- clientsF
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
    }
  }
}
