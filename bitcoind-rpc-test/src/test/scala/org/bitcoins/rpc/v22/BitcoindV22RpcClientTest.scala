package org.bitcoins.rpc.v22

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.util.NodePair
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairV22,
  BitcoindRpcTestUtil
}
import org.scalatest.Assertion

import java.time.ZonedDateTime
import scala.concurrent.Future

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
        blocks <- client.generate(2)
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
  it should "create a descriptor wallet" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    for {
      _ <- client.unloadWallet("")
      _ <- client.createWallet("descriptorWallet", descriptors = true)
      descript <- client.getWalletInfo("descriptorWallet")
      _ <- client.unloadWallet("descriptorWallet")
      _ <- client.loadWallet("")
    } yield {
      descript match {
        case walletInfoPostV22: GetWalletInfoResultPostV22 =>
          assert(walletInfoPostV22.descriptors)
        case _: GetWalletInfoResultPreV22 =>
          fail("descriptors only available on V22 or higher")
      }
    }
  }

  it should "create a wallet with private keys disabled" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      for {
        _ <- client.unloadWallet("")
        _ <- client.createWallet("privKeyWallet", disablePrivateKeys = true)
        walletPriv <- client.getWalletInfo("privKeyWallet")
        _ <- client.unloadWallet("privKeyWallet")
        _ <- client.loadWallet("")
      } yield {
        walletPriv match {
          case walletInfoPostV22: GetWalletInfoResultPostV22 =>
            assert(!walletInfoPostV22.private_keys_enabled)
          case _: GetWalletInfoResultPreV22 =>
            fail("private key parameter only available on V22 or higher")
        }
      }
  }

  it should "output wallet name from listdescriptors" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      for {
        _ <- client.unloadWallet("")
        _ <- client.createWallet("descriptorWalletThree", descriptors = true)
        resultWallets <- client.listDescriptors(walletName =
          "descriptorWalletThree")
        _ <- client.unloadWallet("descriptorWalletThree")
        _ <- client.loadWallet("")
      } yield {
        assert(resultWallets.wallet_name == "descriptorWalletThree")
      }
  }

  it should "output descriptors from listdescriptors" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      for {
        _ <- client.unloadWallet("")
        _ <- client.createWallet("descriptorWalletTwo", descriptors = true)
        resultWallet <- client.listDescriptors(walletName =
          "descriptorWalletTwo")
        _ <- client.unloadWallet("descriptorWalletTwo")
        _ <- client.loadWallet("")
      } yield {
        resultWallet.descriptors.map { d =>
          assert(
            d.desc.isInstanceOf[String] && d.timestamp
              .isInstanceOf[ZonedDateTime]
              && d.active.isInstanceOf[Boolean] && d.internal
                .isInstanceOf[Option[Boolean]]
              && d.range.isInstanceOf[Option[Vector[Int]]] && d.next
                .isInstanceOf[Option[Int]])
        }
        succeed
      }
  }

  it should "be able to decode a reedem script" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val pubKey1 = ecPrivKey1.publicKey
    for {
      _ <- client.unloadWallet("")
      _ <- client.createWallet("decodeRWallet")
      address <- client.getNewAddress(addressType = AddressType.Legacy)
      multisig <-
        client
          .addMultiSigAddress(
            2,
            Vector(Left(pubKey1), Right(address.asInstanceOf[P2PKHAddress])))
      decoded <- client.decodeScript(multisig.redeemScript)
      _ <- client.loadWallet("")
      _ <- client.unloadWallet("decodeRWallet")
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

  it should "output more than one txid" in { nodePair: FixtureParam =>
    val NodePair(client, otherClient) = nodePair
    for {
      blocks <- client.generate(2)
      firstBlock <- client.getBlock(blocks(0))
      transaction0 <- client.getTransaction(firstBlock.tx(0))
      secondBlock <- client.getBlock(blocks(1))
      transaction1 <- client.getTransaction(secondBlock.tx(0))

      address <- otherClient.getNewAddress

      input0 = TransactionOutPoint(transaction0.txid.flip,
                                   UInt32(transaction0.blockindex.get))
      input1 = TransactionOutPoint(transaction1.txid.flip,
                                   UInt32(transaction1.blockindex.get))

      transactionFirst <- {
        val sig: ScriptSignature = ScriptSignature.empty
        val inputs = Vector(TransactionInput(input0, sig, UInt32(1)),
                            TransactionInput(input1, sig, UInt32(2)))
        val outputs = Map(address -> Bitcoins(1))
        client.createRawTransaction(inputs, outputs)
      }
      fundedTransactionOne <- client.fundRawTransaction(transactionFirst)
      signedTransactionOne <- BitcoindRpcTestUtil.signRawTransaction(
        client,
        fundedTransactionOne.hex)

      blocksTwo <- client.generate(2)
      firstBlockTwo <- client.getBlock(blocksTwo(0))
      transaction2 <- client.getTransaction(firstBlockTwo.tx(0))
      secondBlockTwo <- client.getBlock(blocksTwo(1))
      transaction3 <- client.getTransaction(secondBlockTwo.tx(0))

      input2 = TransactionOutPoint(transaction2.txid.flip,
                                   UInt32(transaction2.blockindex.get))
      input3 = TransactionOutPoint(transaction3.txid.flip,
                                   UInt32(transaction3.blockindex.get))

      transactionSecond <- {
        val sig: ScriptSignature = ScriptSignature.empty
        val inputs = Vector(TransactionInput(input2, sig, UInt32(1)),
                            TransactionInput(input3, sig, UInt32(2)))
        val outputs = Map(address -> Bitcoins(1))
        client.createRawTransaction(inputs, outputs)
      }
      fundedTransactionTwo <- client.fundRawTransaction(transactionSecond)
      signedTransactionTwo <- BitcoindRpcTestUtil.signRawTransaction(
        client,
        fundedTransactionTwo.hex)

      _ <- client.generate(100) // Can't spend until depth 100

      mempoolAccept <- client.testMempoolAccept(
        Vector(signedTransactionOne.hex, signedTransactionTwo.hex))
    } yield {
      val mempooltxid: Int = mempoolAccept.length
      assert(mempooltxid > 1)
    }
  }
}
