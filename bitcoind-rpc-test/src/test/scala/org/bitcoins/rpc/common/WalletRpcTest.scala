package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ECPrivateKeyUtil
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  EmptyScriptWitness,
  P2WPKHWitnessV0,
  P2WSHWitnessV0,
  ScriptSignature,
  TaprootWitness
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress, P2PKHAddress}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo.{ECSignatureParams, P2WPKHV0InputInfo}
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  ECPrivateKey,
  ECPublicKey,
  HashType
}
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.{BitcoindInstanceLocal, BitcoindInstanceRemote}
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairV21,
  BitcoindRpcTestUtil
}
import org.bitcoins.testkit.util.AkkaUtil
import org.scalatest.{FutureOutcome, Outcome}

import java.io.File
import java.util.Scanner
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.reflect.io.Directory

class WalletRpcTest extends BitcoindFixturesCachedPairV21 {

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      clients <- clientsF
      futOutcome = with2BitcoindsCached(test, clients)
      fut <- futOutcome.toFuture
    } yield fut

    new FutureOutcome(f)
  }

  // This client's wallet is encrypted
  lazy val walletClientF: Future[BitcoindRpcClient] = clientsF.flatMap { _ =>
    val walletClient =
      BitcoindRpcClient.withActorSystem(
        BitcoindRpcTestUtil.instance(versionOpt = Some(BitcoindVersion.V21)))

    for {
      _ <- startClient(walletClient)
      _ <- walletClient.getNewAddress.flatMap(
        walletClient.generateToAddress(101, _))
      _ <- walletClient.encryptWallet(password)
      _ <- walletClient.stop()
      _ <- RpcUtil.awaitServerShutdown(walletClient)
      // Very rarely we are prevented from starting the client again because Core
      // hasn't released its locks on the datadir. This is prevent that.
      _ <- AkkaUtil.nonBlockingSleep(1.second)
      _ <- walletClient.start()
    } yield walletClient
  }

  var password = "password"

  behavior of "WalletRpc"

  it should "be able to dump the wallet" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val localInstance = client.getDaemon match {
      case _: BitcoindInstanceRemote =>
        sys.error(s"Cannot use remote bitcoind instance in test cases")
      case local: BitcoindInstanceLocal =>
        local
    }
    for {
      result <- {
        val datadir = localInstance.datadir.getAbsolutePath
        client.dumpWallet(datadir + "/test.dat")
      }
    } yield {
      assert(result.filename.exists)
      assert(result.filename.isFile)
    }
  }

  it should "be able to list wallets" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val localInstance = client.getDaemon match {
      case _: BitcoindInstanceRemote =>
        sys.error(s"Cannot use remote bitcoind instance in test cases")
      case local: BitcoindInstanceLocal =>
        local
    }
    for {
      wallets <- client.listWallets
    } yield {

      val expectedFileName =
        if (localInstance.getVersion == BitcoindVersion.V16) "wallet.dat"
        else ""

      assert(wallets == Vector(expectedFileName))
    }
  }

  it should "be able to backup the wallet" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val localInstance = client.getDaemon match {
      case _: BitcoindInstanceRemote =>
        sys.error(s"Cannot use remote bitcoind instance in test cases")
      case local: BitcoindInstanceLocal =>
        local
    }
    for {
      _ <- {
        val datadir = localInstance.datadir.getAbsolutePath
        client.backupWallet(datadir + "/backup.dat")
      }
    } yield {
      val datadir = localInstance.datadir.getAbsolutePath
      val file = new File(datadir + "/backup.dat")
      assert(file.exists)
      assert(file.isFile)
    }
  }

  it should "be able to lock and unlock the wallet" in { _: FixtureParam =>
    for {
      walletClient <- walletClientF
      _ <- walletClient.walletLock()
      _ <- walletClient.walletPassphrase(password, 1000)

      info <- walletClient.getWalletInfo
      _ = assert(info.unlocked_until.nonEmpty)
      _ = assert(info.unlocked_until.get > 0)

      _ <- walletClient.walletLock()

      newInfo <- walletClient.getWalletInfo
    } yield assert(newInfo.unlocked_until.contains(0))
  }

  it should "be able to get an address from bitcoind" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      for {
        _ <- {
          val addrFuts =
            List(client.getNewAddress,
                 client.getNewAddress(AddressType.Bech32),
                 client.getNewAddress(AddressType.P2SHSegwit),
                 client.getNewAddress(AddressType.Legacy))
          Future.sequence(addrFuts)
        }
      } yield succeed
  }

  it should "be able to get a new raw change address" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      for {
        _ <- {
          val addrFuts =
            List(
              client.getRawChangeAddress,
              client.getRawChangeAddress(AddressType.Legacy),
              client.getRawChangeAddress(AddressType.Bech32),
              client.getRawChangeAddress(AddressType.P2SHSegwit)
            )
          Future.sequence(addrFuts)
        }
      } yield succeed
  }

  it should "be able to get the amount recieved by some address" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      for {
        address <- client.getNewAddress
        amount <- client.getReceivedByAddress(address)
      } yield assert(amount == Bitcoins(0))
  }

  it should "be able to get the unconfirmed balance" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      for {
        balance <- client.getUnconfirmedBalance
        transaction <- BitcoindRpcTestUtil.sendCoinbaseTransaction(client,
                                                                   client)
        newBalance <- client.getUnconfirmedBalance
      } yield {
        assert(balance == Bitcoins(0))
        assert(newBalance == transaction.amount)
      }
  }

  it should "be able to get the wallet info" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    for {
      info <- client.getWalletInfo
    } yield {
      assert(info.balance.toBigDecimal > 0)
      assert(info.txcount > 0)
      assert(info.keypoolsize > 0)
      assert(!info.unlocked_until.contains(0))
    }
  }

  it should "be able to refill the keypool" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    for {
      info <- client.getWalletInfo
      _ <- client.keyPoolRefill(info.keypoolsize + 1)
      newInfo <- client.getWalletInfo
    } yield assert(newInfo.keypoolsize == info.keypoolsize + 1)
  }

  it should "be able to change the wallet password" in { _: FixtureParam =>
    val newPass = "new_password"

    for {
      walletClient <- walletClientF
      _ <- walletClient.walletLock()
      _ <- walletClient.walletPassphraseChange(password, newPass)
      _ = {
        password = newPass
      }

      _ <- walletClient.walletPassphrase(password, 1000)
      info <- walletClient.getWalletInfo
      _ <- walletClient.walletLock()
      newInfo <- walletClient.getWalletInfo
    } yield {

      assert(info.unlocked_until.nonEmpty)
      assert(info.unlocked_until.get > 0)
      assert(newInfo.unlocked_until.contains(0))
    }
  }

  it should "be able to list address groupings" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    val amount = Bitcoins(1.25)

    def getChangeAddressAndAmount(
        client: BitcoindRpcClient,
        address: BitcoinAddress,
        txid: DoubleSha256DigestBE): Future[(BitcoinAddress, CurrencyUnit)] = {
      for {
        rawTx <- client.getRawTransactionRaw(txid)
      } yield {
        val outs = rawTx.outputs.filterNot(_.value == amount)
        val changeAddresses = outs
          .map(out =>
            (BitcoinAddress.fromScriptPubKey(out.scriptPubKey, networkParam),
             out.value))
        assert(changeAddresses.size == 1)
        assert(changeAddresses.head._1 != address)
        (changeAddresses.head._1, changeAddresses.head._2)
      }
    }

    for {
      groupingsBefore <- client.listAddressGroupings

      address <- client.getNewAddress

      txid <- BitcoindRpcTestUtil.fundBlockChainTransaction(client,
                                                            otherClient,
                                                            address,
                                                            amount)

      (changeAddress, changeAmount) <-
        getChangeAddressAndAmount(client, address, txid)

      groupingsAfter <- client.listAddressGroupings
    } yield {

      // the address should appear in a new address grouping
      assert(!groupingsBefore.exists(vec => vec.exists(_.address == address)))

      val rpcAddress =
        groupingsAfter.find(vec => vec.exists(_.address == address)).get.head
      assert(rpcAddress.address == address)
      assert(rpcAddress.balance == amount)

      // the change address should be added to an exiting address grouping
      assert(
        !groupingsBefore.exists(vec => vec.exists(_.address == changeAddress)))

      val changeGroupingOpt =
        groupingsAfter.find(vec => vec.exists(_.address == changeAddress))
      assert(changeGroupingOpt.nonEmpty)

      val changeGrouping = changeGroupingOpt.get
      assert(changeGrouping.size > 1)

      val rpcChangeAddress =
        changeGrouping.find(addr => addr.address == changeAddress).get
      assert(rpcChangeAddress.address == changeAddress)
      assert(rpcChangeAddress.balance == changeAmount)
    }
  }

  it should "be able to send to an address" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
      address <- otherClient.getNewAddress
      txid <- client.sendToAddress(address, Bitcoins(1))
      transaction <- client.getTransaction(txid)
    } yield {
      assert(transaction.amount == Bitcoins(-1))
      assert(transaction.details.head.address.contains(address))
    }
  }

  it should "be able to send btc to many addresses" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val otherClient = nodePair.node2
      for {
        address1 <- otherClient.getNewAddress
        address2 <- otherClient.getNewAddress
        txid <-
          client
            .sendMany(Map(address1 -> Bitcoins(1), address2 -> Bitcoins(2)))
        transaction <- client.getTransaction(txid)
      } yield {
        assert(transaction.amount == Bitcoins(-3))
        assert(transaction.details.exists(_.address.contains(address1)))
        assert(transaction.details.exists(_.address.contains(address2)))
      }
  }

  it should "be able to list transactions by receiving addresses" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val otherClient = nodePair.node2
      for {
        address <- otherClient.getNewAddress
        txid <-
          BitcoindRpcTestUtil
            .fundBlockChainTransaction(client,
                                       otherClient,
                                       address,
                                       Bitcoins(1.5))
        receivedList <- otherClient.listReceivedByAddress()
      } yield {
        val entryList =
          receivedList.filter(entry => entry.address == address)
        assert(entryList.length == 1)
        val entry = entryList.head
        assert(entry.txids.head == txid)
        assert(entry.address == address)
        assert(entry.amount == Bitcoins(1.5))
        assert(entry.confirmations == 1)
      }
  }

  it should "be able to import an address" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    val address = Bech32Address
      .fromString("bcrt1q9h9wkz6ad49szfl035wh3qdacuslkp6j9pfp4j")

    for {
      _ <- otherClient.importAddress(address)
      txid <- BitcoindRpcTestUtil.fundBlockChainTransaction(client,
                                                            otherClient,
                                                            address,
                                                            Bitcoins(1.5))
      list <- otherClient.listReceivedByAddress(includeWatchOnly = true)
    } yield {
      val entry =
        list
          .find(_.involvesWatchonly.contains(true))
          .get
      assert(entry.address == address)
      assert(entry.involvesWatchonly.contains(true))
      assert(entry.amount == Bitcoins(1.5))
      assert(entry.txids.head == txid)
    }
  }

  it should "be able to get the balance" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    for {
      balance <- client.getBalance
      _ <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
      newBalance <- client.getBalance
    } yield {
      assert(balance.toBigDecimal > 0)
      assert(balance.toBigDecimal < newBalance.toBigDecimal)
    }
  }

  it should "be able to dump a private key" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    for {
      address <- client.getNewAddress
      _ <- client.dumpPrivKey(address)
    } yield succeed
  }

  it should "be able to import a private key" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val ecPrivateKey = ECPrivateKey.freshPrivateKey
    val publicKey = ecPrivateKey.publicKey
    val address = P2PKHAddress(publicKey, networkParam)
    val localInstance = client.getDaemon match {
      case _: BitcoindInstanceRemote =>
        sys.error(s"Cannot use remote bitcoind instance in test cases")
      case local: BitcoindInstanceLocal =>
        local
    }
    for {
      _ <- client.importPrivKey(ecPrivateKey.toPrivateKeyBytes(),
                                rescan = false)
      key <- client.dumpPrivKey(address)
      result <-
        client
          .dumpWallet(
            localInstance.datadir.getAbsolutePath + "/wallet_dump.dat")
    } yield {
      assert(key.toPrivateKey == ecPrivateKey)
      val reader = new Scanner(result.filename)
      var found = false
      while (reader.hasNext) {
        if (
          reader.next == ECPrivateKeyUtil.toWIF(
            ecPrivateKey.toPrivateKeyBytes(),
            networkParam)
        ) {
          found = true
        }
      }
      assert(found)
    }
  }

  it should "be able to import a public key" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val pubKey = ECPublicKey.freshPublicKey
    for {
      _ <- client.importPubKey(pubKey)
    } yield succeed
  }

  it should "be able to import multiple addresses with importMulti" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val privKey = ECPrivateKey.freshPrivateKey
      val address1 = P2PKHAddress(privKey.publicKey, networkParam)

      val privKey1 = ECPrivateKey.freshPrivateKey
      val privKey2 = ECPrivateKey.freshPrivateKey

      for {
        firstResult <-
          client
            .createMultiSig(2, Vector(privKey1.publicKey, privKey2.publicKey))
        address2 = firstResult.address

        secondResult <-
          client
            .importMulti(
              Vector(
                RpcOpts.ImportMultiRequest(RpcOpts.ImportMultiAddress(address1),
                                           UInt32(0)),
                RpcOpts.ImportMultiRequest(RpcOpts.ImportMultiAddress(address2),
                                           UInt32(0))),
              rescan = false
            )
      } yield {
        assert(secondResult.length == 2)
        assert(secondResult(0).success)
        assert(secondResult(1).success)
      }
  }

  it should "be able to import a wallet" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val localInstance = client.getDaemon match {
      case _: BitcoindInstanceRemote =>
        sys.error(s"Cannot use remote bitcoind instance in test cases")
      case local: BitcoindInstanceLocal =>
        local
    }
    for {
      walletClient <- walletClientF
      address <- client.getNewAddress
      walletFile =
        localInstance.datadir.getAbsolutePath + "/client_wallet.dat"

      fileResult <- client.dumpWallet(walletFile)
      _ <- walletClient.walletPassphrase(password, 1000)
      _ <- walletClient.importWallet(walletFile)
      _ <- walletClient.dumpPrivKey(address)
    } yield assert(fileResult.filename.exists)

  }

  it should "be able to load a wallet" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val name = "tmp_wallet"
    val localInstance = client.getDaemon match {
      case _: BitcoindInstanceRemote =>
        sys.error(s"Cannot use remote bitcoind instance in test cases")
      case local: BitcoindInstanceLocal =>
        local
    }
    for {
      walletClient <- walletClientF
      walletFile =
        localInstance.datadir.getAbsolutePath + s"/regtest/wallets/$name"

      _ <- client.createWallet(walletFile)
      _ <- client.unloadWallet(walletFile)
      loadResult <- walletClient.loadWallet(walletFile)
    } yield {
      // clean up
      val directory = new Directory(new File(walletFile))
      directory.deleteRecursively()

      assert(loadResult.name == walletFile)
    }
  }

  it should "be able to set the tx fee" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    for {
      success <- client.setTxFee(Bitcoins(0.01))
      info <- client.getWalletInfo
    } yield {
      assert(success)
      assert(info.paytxfee == SatoshisPerByte(Satoshis(1000)))
    }
  }

  it should "be able to bump a mem pool tx fee" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
      address <- otherClient.getNewAddress
      unspent <- client.listUnspent
      changeAddress <- client.getRawChangeAddress
      rawTx <- {
        val output =
          unspent.find(output => output.amount.toBigDecimal > 1).get
        val input =
          TransactionInput(
            TransactionOutPoint(output.txid.flip, UInt32(output.vout)),
            ScriptSignature.empty,
            UInt32.max - UInt32(2))
        val inputs = Vector(input)

        val outputs =
          Map(address -> Bitcoins(0.5),
              changeAddress -> Bitcoins(output.amount.toBigDecimal - 0.55))

        client.createRawTransaction(inputs, outputs)
      }
      stx <- BitcoindRpcTestUtil.signRawTransaction(client, rawTx)
      txid <- client.sendRawTransaction(stx.hex, 0)
      tx <- client.getTransaction(txid)
      bumpedTx <- client.bumpFee(txid)
    } yield assert(tx.fee.get < bumpedTx.fee)
  }

  it should "be able to sign a raw transaction with the wallet" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val otherClient = nodePair.node2
      for {
        address <- otherClient.getNewAddress
        transactionWithoutFunds <-
          client
            .createRawTransaction(Vector.empty, Map(address -> Bitcoins(1)))
        transactionResult <- client.fundRawTransaction(transactionWithoutFunds)
        transaction = transactionResult.hex
        singedTx <- client.signRawTransactionWithWallet(transaction).map(_.hex)

        // Will throw error if invalid
        _ <- client.sendRawTransaction(singedTx)
      } yield {
        assert(transaction.inputs.length == 2)
        assert(
          transaction.outputs.contains(
            TransactionOutput(Bitcoins(1), address.scriptPubKey)))
      }
  }

  it should "generate the same (low R) signatures as bitcoin-s" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val otherClient = nodePair.node2
      for {
        address <- otherClient.getNewAddress
        transactionWithoutFunds <-
          client
            .createRawTransaction(Vector.empty, Map(address -> Bitcoins(1)))
        transactionResult <- client.fundRawTransaction(transactionWithoutFunds)
        transaction = transactionResult.hex
        signedTx <- client.signRawTransactionWithWallet(transaction).map(_.hex)

        // Validate signature against bitcoin-s generated one
        outPoint = transaction.inputs.head.previousOutput
        prevTx <- client.getRawTransactionRaw(outPoint.txIdBE)
        output = prevTx.outputs(outPoint.vout.toInt)
        privKey <- client.dumpPrivKey(
          BitcoinAddress.fromScriptPubKey(output.scriptPubKey, RegTest))
      } yield {
        val partialSig = BitcoinSigner.signSingle(
          ECSignatureParams(P2WPKHV0InputInfo(outPoint,
                                              output.value,
                                              privKey.toPrivateKey.publicKey),
                            prevTx,
                            privKey.toPrivateKey,
                            HashType.sigHashAll),
          transaction,
          isDummySignature = false
        )

        signedTx match {
          case btx: NonWitnessTransaction =>
            assert(
              btx.inputs.head.scriptSignature.signatures.head == partialSig.signature)
          case wtx: WitnessTransaction =>
            wtx.witness.head match {
              case p2wpkh: P2WPKHWitnessV0 =>
                assert(p2wpkh.pubKey == partialSig.pubKey)
                assert(p2wpkh.signature == partialSig.signature)
              case _: P2WSHWitnessV0 | EmptyScriptWitness | _: TaprootWitness =>
                fail("Expected P2WPKH")
              case _: TaprootWitness =>
                throw new UnsupportedOperationException(
                  s"Taproot not implemented")
            }
        }
      }
  }

  def startClient(client: BitcoindRpcClient): Future[Unit] = {
    BitcoindRpcTestUtil.startServers(Vector(client))
  }

  override def afterAll(): Unit = {
    val stopF = walletClientF.map(BitcoindRpcTestUtil.stopServer)
    Await.result(stopF, duration)
    super.afterAll()
  }
}
