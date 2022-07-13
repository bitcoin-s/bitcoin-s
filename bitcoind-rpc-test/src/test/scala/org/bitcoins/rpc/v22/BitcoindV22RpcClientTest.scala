package org.bitcoins.rpc.v22

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairV22,
  BitcoindRpcTestUtil
}
import org.scalatest.Assertion
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

import scala.concurrent.{Await, Future}

class BitcoindV22RpcClientTest extends BitcoindFixturesCachedPairV22 {

  behavior of "BitcoindV22RpcClient"

  it should "be able to start a V22 bitcoind instance" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      for {
        v <- client.version
      } yield assert(v == BitcoindVersion.V22)
  }

  it should "be able to get network info" in { nodePair: FixtureParam =>
    val freshClient = nodePair.node1
    for {
      info <- freshClient.getNetworkInfo
    } yield {
      assert(info.networkactive)
      assert(info.localrelay)
    }
  }

  it should "be able to decode a reedem script" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val pubKey1 = ecPrivKey1.publicKey
    for {
      address <- client.getNewAddress(addressType = AddressType.Legacy)
      multisig <-
        client
          .addMultiSigAddress(
            2,
            Vector(Left(pubKey1), Right(address.asInstanceOf[P2PKHAddress])))
      decoded <- client.decodeScript(multisig.redeemScript)
    } yield {
      decoded match {
        case decodedPreV22: DecodeScriptResultPreV22 =>
          assert(decodedPreV22.reqSigs.contains(2))
          assert(decoded.typeOfScript.contains(ScriptType.MULTISIG))
          assert(decodedPreV22.addresses.get.contains(address))
        case decodedV22: DecodeScriptResultV22 =>
          assert(decodedV22.typeOfScript.contains(ScriptType.MULTISIG))
      }

    }
  }

  it should "be able to decode a raw transaction" in { nodePair: FixtureParam =>
    val client1 = nodePair.node1
    val client2 = nodePair.node2
    for {
      transaction <-
        BitcoindRpcTestUtil
          .createRawCoinbaseTransaction(client1, client2)
      rpcTransaction <- client1.decodeRawTransaction(transaction)
    } yield {
      assert(rpcTransaction.txid == transaction.txIdBE)
      assert(rpcTransaction.locktime == transaction.lockTime)
      assert(rpcTransaction.size == transaction.byteSize)
      assert(rpcTransaction.version == transaction.version.toInt)
      assert(rpcTransaction.vsize == transaction.vsize)
    }
  }

  it should "be able to get a raw transaction using both rpcs available" in {
    nodePair: FixtureParam =>
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

  it should "be able to get utxo info" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      info1 <- client.getTxOut(block.tx.head.txid, 0)
    } yield assert(info1.coinbase)
  }

  it should "decode all the BIP174 example PSBTs" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val psbts = Vector(
      "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",
      "cHNidP8BAFUCAAAAASeaIyOl37UfxF8iD6WLD8E+HjNCeSqF1+Ns1jM7XLw5AAAAAAD/////AaBa6gsAAAAAGXapFP/pwAYQl8w7Y28ssEYPpPxCfStFiKwAAAAAAAEBIJVe6gsAAAAAF6kUY0UgD2jRieGtwN8cTRbqjxTA2+uHIgIDsTQcy6doO2r08SOM1ul+cWfVafrEfx5I1HVBhENVvUZGMEMCIAQktY7/qqaU4VWepck7v9SokGQiQFXN8HC2dxRpRC0HAh9cjrD+plFtYLisszrWTt5g6Hhb+zqpS5m9+GFR25qaAQEEIgAgdx/RitRZZm3Unz1WTj28QvTIR3TjYK2haBao7UiNVoEBBUdSIQOxNBzLp2g7avTxI4zW6X5xZ9Vp+sR/HkjUdUGEQ1W9RiED3lXR4drIBeP4pYwfv5uUwC89uq/hJ/78pJlfJvggg71SriIGA7E0HMunaDtq9PEjjNbpfnFn1Wn6xH8eSNR1QYRDVb1GELSmumcAAACAAAAAgAQAAIAiBgPeVdHh2sgF4/iljB+/m5TALz26r+En/vykmV8m+CCDvRC0prpnAAAAgAAAAIAFAACAAAA=",
      "cHNidP8BAD8CAAAAAf//////////////////////////////////////////AAAAAAD/////AQAAAAAAAAAAA2oBAAAAAAAACg8BAgMEBQYHCAkPAQIDBAUGBwgJCgsMDQ4PAAA=",
      "cHNidP8BACoCAAAAAAFAQg8AAAAAABepFG6Rty1Vk+fUOR4v9E6R6YXDFkHwhwAAAAAAAA==" // this one is from Core
    ).map(PSBT.fromBase64)

    for {
      _ <- Future.sequence(psbts.map(client.decodePsbt))
    } yield succeed
  }

  it should "be able to get a block with verbose transactions" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      for {
        blocks <- client.getNewAddress.flatMap(client.generateToAddress(2, _))
        block <- client.getBlockWithTransactions(blocks(1))
      } yield {
        assert(block.hash == blocks(1))
        assert(block.tx.length == 1)
        val tx = block.tx.head
        assert(tx.vout.head.n == 0)
      }
  }

  it should "take a network input and output addresses of same network" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val networkOption = Vector("ipv4", "ipv6", "onion", "i2p")
      val resultNested: Vector[Future[Unit]] = networkOption.map {
        networkType =>
          val resultVecF: Future[Vector[GetNodeAddressesResultPostV22]] =
            client.getNodeAddresses(networkType, 10)
          resultVecF.map { resultVec =>
            resultVec.foreach { result =>
              assert(result.network == networkType)
            }
          }
      }
      val result: Future[Assertion] = Future
        .sequence(resultNested)
        .map(_ => succeed)
      result
  }

  it should "return a network address" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val resultVecF: Future[Vector[GetNodeAddressesResultPostV22]] =
      client.getNodeAddresses()
    resultVecF.map { resultVec =>
      resultVec.foreach { result =>
        assert(
          result.network == "ipv4" || result.network == "ipv6" || result.network == "onion" || result.network == "i2p")
      }
      succeed
    }
  }

  /**        it should "output descriptors" in { nodePair: FixtureParam =>
    *              val client=nodePair.node1
    *              client.createWallet("descriptorWallet", descriptors = true )
    *                val resultWalletF: Future[Vector[listDescriptorsResult]] =
    *                client.listDescriptors()
    *                resultWalletF.map{resultWallet =>
    *                  resultWallet.foreach {result =>
    *                    result.descriptors.map{ descrResult => assert(descrResult.desc.isInstanceOf[String] &&
    *                    descrResult.timestamp.isInstancesOf[Int] && descrResult.active.isInstanceOf[Boolean] &&
    *                    descrResult.internal.isInstanceOf[Boolean] && descrResult.range.isInstanceOf[Array[(Int,Int)]]
    *                    && descrResult.next.isInstanceOf[Int]
    *                    )
    *          }
    *                    assert (result.wallet_name == "descriptorWallet" && result.descriptors.desc.isDefined
    *                      && result.descriptors.timestamp.isInstanceOf[Int] && result.descriptors.active.isInstanceOf[Boolean]
    *                      && result.descriptors.internal.isInstanceOf[Boolean] && result.descriptors.range.isInstanceOf[Array[(Int,Int)]] &&
    *                      result.descriptors.next.isInstanceOf[Int]
    *              )
    *                  }
    *              }
    *           }
    */

  it should "output descriptors" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val walletsUF: Future[Vector[String]] = client.listWallets
    val walletsUnload: Future[Vector[Unit]] = walletsUF.map { walletsU =>
      val walletsUI: Future[Vector[Unit]] =
        Future.sequence(walletsU.map { wallet =>
          client.unloadWallet(wallet)
        })
      walletsUI
    }.flatten
    Await.ready(walletsUnload, 3.seconds)
    val descriptorWallet: Future[CreateWalletResult] =
      client.createWallet("descriptorWallet", descriptors = true)
    Await.ready(descriptorWallet, 3.seconds)
    val resultWalletF: Future[Vector[listDescriptorsResult]] =
      client.listDescriptors()
    descriptorWallet.flatMap { _ =>
      resultWalletF.map { resultWallet =>
        resultWallet.map { result =>
          assert(result.wallet_name == "descriptorWallet")
        }
        succeed
      }
    }
  }

}
