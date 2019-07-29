package org.bitcoins.rpc.v17

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{DoubleSha256DigestBE, ECPrivateKey}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionInput
import org.bitcoins.rpc.client.common.RpcOpts.{
  AddressType,
  LabelPurpose,
  SignRawTransactionOutputParameter
}
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.util.AsyncUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest
import java.time.LocalDateTime

import scala.concurrent.Future

class BitcoindV17RpcClientTest extends BitcoindRpcTest {
  val usedLabel = "used_label"
  val unusedLabel = "unused_label"

  val clientsF: Future[(BitcoindV17RpcClient, BitcoindV17RpcClient)] =
    BitcoindRpcTestUtil.createNodePairV17(clientAccum)

  behavior of "BitcoindV17RpcClient"

  it should "test mempool acceptance" in {
    for {
      (client, otherClient) <- clientsF
      tx <- BitcoindRpcTestUtil.createRawCoinbaseTransaction(client,
                                                             otherClient)
      acceptance <- client.testMempoolAccept(tx)
    } yield {
      assert(acceptance.rejectReason.isEmpty == acceptance.allowed)
    }
  }

  it should "sign a raw transaction with wallet keys" in {
    for {
      (client, otherClient) <- clientsF
      rawTx <- BitcoindRpcTestUtil.createRawCoinbaseTransaction(client,
                                                                otherClient)
      signedTx <- client.signRawTransactionWithWallet(rawTx)
    } yield assert(signedTx.complete)
  }

  // copied from Bitcoin Core: https://github.com/bitcoin/bitcoin/blob/fa6180188b8ab89af97860e6497716405a48bab6/test/functional/rpc_signrawtransaction.py
  it should "sign a raw transaction with private keys" in {
    val privkeys =
      List("cUeKHd5orzT3mz8P9pxyREHfsWtVfgsfDjiZZBcjUBAaGk1BTj7N",
           "cVKpPfVKSJxKqVpE9awvXNWuLHCa5j5tiE7K6zbUSptFpTEtiFrA")
        .map(ECPrivateKey.fromWIFToPrivateKey)

    val txids =
      List("9b907ef1e3c26fc71fe4a4b3580bc75264112f95050014157059c736f0202e71",
           "83a4f6a6b73660e13ee6cb3c6063fa3759c50c9b7521d0536022961898f4fb02")
        .map(DoubleSha256DigestBE.fromHex)

    val vouts = List(0, 0)

    val inputs: Vector[TransactionInput] = txids
      .zip(vouts)
      .map {
        case (txid, vout) =>
          TransactionInput.fromTxidAndVout(txid, UInt32(vout))
      }
      .toVector

    val address =
      BitcoinAddress.fromStringExn("mpLQjfK79b7CCV4VMJWEWAj5Mpx8Up5zxB")

    val outputs: Map[BitcoinAddress, Bitcoins] =
      Map(address -> Bitcoins(0.1))

    val scriptPubKeys =
      List("76a91460baa0f494b38ce3c940dea67f3804dc52d1fb9488ac",
           "76a914669b857c03a5ed269d5d85a1ffac9ed5d663072788ac")
        .map(ScriptPubKey.fromAsmHex)

    val utxoDeps = inputs.zip(scriptPubKeys).map {
      case (input, pubKey) =>
        SignRawTransactionOutputParameter.fromTransactionInput(input, pubKey)
    }

    for {
      (client, _) <- clientsF
      rawTx <- client.createRawTransaction(inputs, outputs)
      signed <- client.signRawTransactionWithKey(rawTx,
                                                 privkeys.toVector,
                                                 utxoDeps)
    } yield assert(signed.complete)
  }

  it should "be able to get the address info for a given address" in {
    for {
      (client, _) <- clientsF
      addr <- client.getNewAddress
      info <- client.getAddressInfo(addr)
    } yield
      assert(
        info.timestamp.exists(_.getDayOfYear == LocalDateTime.now.getDayOfYear))
  }

  it should "be able to get the address info for a given P2SHSegwit address" in {
    for {
      (client, _) <- clientsF
      addr <- client.getNewAddress(addressType = AddressType.P2SHSegwit)
      info <- client.getAddressInfo(addr)
    } yield
      assert(
        info.timestamp.exists(_.getDayOfYear == LocalDateTime.now.getDayOfYear))
  }

  it should "be able to get the address info for a given Legacy address" in {
    for {
      (client, _) <- clientsF
      addr <- client.getNewAddress(addressType = AddressType.Legacy)
      info <- client.getAddressInfo(addr)
    } yield
      assert(
        info.timestamp.exists(_.getDayOfYear == LocalDateTime.now.getDayOfYear))
  }

  // needs #360 to be merged
  it should "be able to get the address info for a given Bech32 address" in {
    for {
      (client, _) <- clientsF
      addr <- client.getNewAddress(AddressType.Bech32)
      info <- client.getAddressInfo(addr)
    } yield {
      assert(info.address.networkParameters == RegTest)
      assert(
        info.timestamp.exists(_.getDayOfYear == LocalDateTime.now.getDayOfYear))
    }
  }

  it should "be able to get the amount received by a label" in {
    for {
      (client, _) <- clientsF
      address <- client.getNewAddress(usedLabel)
      _ <- BitcoindRpcTestUtil
        .fundBlockChainTransaction(client, address, Bitcoins(1.5))

      amount <- client.getReceivedByLabel(usedLabel)
    } yield assert(amount == Bitcoins(1.5))
  }

  it should "list all labels" in {
    for {
      (client, _) <- clientsF
      _ <- client.listLabels()
    } yield succeed
  }

  it should "list all labels with purposes" in {
    clientsF.flatMap {
      case (client, otherClient) =>
        val sendLabel = "sendLabel"

        val isImportDone = () =>
          client.ping().map(_ => true).recover {
            case exc if exc.getMessage.contains("rescanning") => false
            case exc =>
              logger.error(s"throwing $exc")
              throw exc
        }

        def importTx(n: Int): Future[Unit] =
          for {
            address <- otherClient.getNewAddress
            _ <- client.importAddress(address, sendLabel + n)
            _ <- AsyncUtil.retryUntilSatisfiedF(isImportDone)
          } yield ()

        for {
          _ <- importTx(0)
          _ <- importTx(1)
          receiveLabels <- client.listLabels(Some(LabelPurpose.Receive))
          sendLabels <- client.listLabels(Some(LabelPurpose.Send))
        } yield assert(receiveLabels != sendLabels)
    }
  }

  it should "set labels" in {
    val l = "setLabel"
    val btc = Bitcoins(1)
    for {
      (client, otherClient) <- clientsF
      addr <- client.getNewAddress
      _ <- BitcoindRpcTestUtil.fundBlockChainTransaction(otherClient, addr, btc)

      newestBlock <- otherClient.getBestBlockHash
      _ <- AsyncUtil.retryUntilSatisfiedF(() =>
        BitcoindRpcTestUtil.hasSeenBlock(client, newestBlock))

      oldAmount <- client.getReceivedByLabel(l)
      _ = assert(oldAmount == Bitcoins(0))
      _ <- client.setLabel(addr, l)
      newAmount <- client.getReceivedByLabel(l)

    } yield assert(newAmount == btc)
  }

  it should "list amounts received by all labels" in {
    for {
      (client, otherClient) <- clientsF
      addressWithLabel <- client.getNewAddress(usedLabel)
      addressNoLabel <- client.getNewAddress
      _ <- otherClient.sendToAddress(addressNoLabel, Bitcoins.one)
      _ <- otherClient.sendToAddress(addressWithLabel, Bitcoins.one)
      newBlock +: _ <- otherClient.generate(1)
      _ <- AsyncUtil.retryUntilSatisfiedF(() =>
        BitcoindRpcTestUtil.hasSeenBlock(client, newBlock))
      list <- client.listReceivedByLabel()
    } yield {

      val receivedToUsedlabel = list.find(_.label == usedLabel)

      assert(receivedToUsedlabel.isDefined)
      assert(receivedToUsedlabel.get.amount > Bitcoins.zero)

      val receivedDefaultLabel =
        list
          .find(_.label == "")

      assert(receivedDefaultLabel.isDefined)
      assert(receivedDefaultLabel.get.amount > Bitcoins.zero)

      assert(list.forall(_.label != unusedLabel))
    }
  }
}
