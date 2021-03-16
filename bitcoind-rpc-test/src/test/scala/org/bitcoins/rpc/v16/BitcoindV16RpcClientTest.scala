package org.bitcoins.rpc.v16

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.SignRawTransactionOutputParameter
import org.bitcoins.core.crypto.ECPrivateKeyUtil
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction.{
  TransactionConstants,
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.crypto.{DoubleSha256DigestBE, ECPrivateKey}
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairV16,
  BitcoindRpcTestUtil
}

import scala.concurrent.duration.DurationInt

class BitcoindV16RpcClientTest extends BitcoindFixturesCachedPairV16 {

  behavior of "BitcoindV16RpcClient"

  it should "be able to get peer info" in { nodePair: FixtureParam =>
    val freshClient = nodePair.node1
    val otherFreshClient = nodePair.node2
    for {
      infoList <- freshClient.getPeerInfo
    } yield {
      assert(infoList.length >= 0)
      val info = infoList.head
      assert(info.addnode)
      assert(info.networkInfo.addr == otherFreshClient.getDaemon.uri)
    }
  }

  it should "be able to start a V16 bitcoind" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    assert(client.version == BitcoindVersion.V16)
    assert(otherClient.version == BitcoindVersion.V16)
  }

  it should "be able to sign a raw transaction" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
      addr <- client.getNewAddress
      _ <- otherClient.sendToAddress(addr, Bitcoins.one)
      _ <-
        otherClient.getNewAddress.flatMap(otherClient.generateToAddress(6, _))
      peers <- client.getPeerInfo
      _ = assert(peers.exists(_.networkInfo.addr == otherClient.getDaemon.uri))

      recentBlock <- otherClient.getBestBlockHash
      _ <- AsyncUtil.retryUntilSatisfiedF(
        () => BitcoindRpcTestUtil.hasSeenBlock(client, recentBlock),
        1.second)
      (utxoTxid, utxoVout) <-
        client.listUnspent
          .map(_.filter(_.address.contains(addr)))
          .map(_.head)
          .map(utxo => (utxo.txid, utxo.vout))
      newAddress <- client.getNewAddress
      rawTx <- {
        val outPoint = TransactionOutPoint(utxoTxid.flip, UInt32(utxoVout))
        val input = TransactionInput(outPoint,
                                     ScriptSignature.empty,
                                     TransactionConstants.sequence)
        val outputs = Map(newAddress -> Bitcoins(0.5))

        client.createRawTransaction(Vector(input), outputs)
      }
      signedRawTx <- client.signRawTransaction(rawTx)
    } yield {
      assert(signedRawTx.complete)
    }
  }

  // copied form the equivalent test in BitcoindV17RpcClientTest
  it should "be able to sign a raw transaction with private keys" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val privkeys: List[ECPrivateKey] =
        List("cUeKHd5orzT3mz8P9pxyREHfsWtVfgsfDjiZZBcjUBAaGk1BTj7N",
             "cVKpPfVKSJxKqVpE9awvXNWuLHCa5j5tiE7K6zbUSptFpTEtiFrA")
          .map(ECPrivateKeyUtil.fromWIFToPrivateKey)

      val txids =
        List("9b907ef1e3c26fc71fe4a4b3580bc75264112f95050014157059c736f0202e71",
             "83a4f6a6b73660e13ee6cb3c6063fa3759c50c9b7521d0536022961898f4fb02")
          .map(DoubleSha256DigestBE.fromHex)

      val vouts = List(0, 0)

      val inputs: Vector[TransactionInput] = txids
        .zip(vouts)
        .map { case (txid, vout) =>
          TransactionInput.fromTxidAndVout(txid, UInt32(vout))
        }
        .toVector

      val address =
        BitcoinAddress.fromString("mpLQjfK79b7CCV4VMJWEWAj5Mpx8Up5zxB")

      val outputs: Map[BitcoinAddress, Bitcoins] =
        Map(address -> Bitcoins(0.1))

      val scriptPubKeys =
        List("76a91460baa0f494b38ce3c940dea67f3804dc52d1fb9488ac",
             "76a914669b857c03a5ed269d5d85a1ffac9ed5d663072788ac")
          .map(ScriptPubKey.fromAsmHex)

      val utxoDeps = inputs.zip(scriptPubKeys).map { case (input, pubKey) =>
        SignRawTransactionOutputParameter.fromTransactionInput(input, pubKey)
      }

      for {
        rawTx <- client.createRawTransaction(inputs, outputs)
        signed <- client.signRawTransaction(rawTx, utxoDeps, privkeys.toVector)
      } yield assert(signed.complete)
  }

  it should "be able to send from an account to an addresss" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val otherClient = nodePair.node2
      for {
        address <- otherClient.getNewAddress
        txid <- client.sendFrom("", address, Bitcoins(1))
        transaction <- client.getTransaction(txid)
      } yield {
        assert(transaction.amount == Bitcoins(-1))
        assert(transaction.details.head.address.contains(address))
      }
  }

  it should "be able to get and set the account for a given address" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val account1 = "account_1"
      val account2 = "account_2"
      for {
        address <- client.getNewAddress(account1)
        acc1 <- client.getAccount(address)
        _ <- client.setAccount(address, account2)
        acc2 <- client.getAccount(address)
      } yield {
        assert(acc1 == account1)
        assert(acc2 == account2)
      }
  }

  it should "be able to get all addresses belonging to an account" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      for {
        address <- client.getNewAddress
        addresses <- client.getAddressesByAccount("")
      } yield assert(addresses.contains(address))
  }

  it should "be able to get an account's address" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val account = "a_new_account"
    for {
      address <- client.getAccountAddress(account)
      result <- client.getAccount(address)
    } yield assert(result == account)
  }

  it should "be able to move funds from one account to another" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val account = "move_account"
      for {
        success <- client.move("", account, Bitcoins(1))
        map <- client.listAccounts()
      } yield {
        assert(success)
        assert(map(account) == Bitcoins(1))
      }
  }
}
