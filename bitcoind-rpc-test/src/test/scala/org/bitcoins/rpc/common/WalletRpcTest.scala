package org.bitcoins.rpc.common

import org.bitcoins.commons.file.FileUtil
import org.bitcoins.commons.jsonmodels.bitcoind.GetWalletInfoResultPostV22
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{
  AddressType,
  WalletFlag
}
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.script.descriptor.P2WPKHDescriptor
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{
  Bech32Address,
  Bech32mAddress,
  BitcoinAddress
}
import org.bitcoins.core.psbt.PSBT
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
  BitcoindFixturesCachedPairNewest,
  BitcoindRpcTestUtil
}
import org.bitcoins.testkit.util.PekkoUtil
import org.scalatest.{FutureOutcome, Outcome}

import java.io.File
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

class WalletRpcTest extends BitcoindFixturesCachedPairNewest {

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
        BitcoindRpcTestUtil.instance(versionOpt = Some(BitcoindVersion.newest))
      )

    for {
      _ <- startClient(walletClient)
      _ <- walletClient.generate(101)
      _ <- walletClient.encryptWallet(password)
      _ <- walletClient.stop()
      _ <- RpcUtil.awaitServerShutdown(walletClient)
      // Very rarely we are prevented from starting the client again because Core
      // hasn't released its locks on the datadir. This is prevent that.
      _ <- PekkoUtil.nonBlockingSleep(1.second)
      _ <- walletClient.start()
    } yield walletClient
  }

  var password = "password"

  behavior of "WalletRpc"

  it should "be able to list wallets" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    for {
      wallets <- client.listWallets
    } yield {

      val expectedFileName = ""

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
            List(
              client.getNewAddress,
              client.getNewAddress(AddressType.Bech32),
              client.getNewAddress(AddressType.P2SHSegwit),
              client.getNewAddress(AddressType.Legacy)
            )
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
        transaction <- BitcoindRpcTestUtil.sendCoinbaseTransaction(
          client,
          client
        )
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

  it should "be able to refill the keypool" ignore { nodePair: FixtureParam =>
    // ignore until: https://github.com/bitcoin/bitcoin/issues/29924
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
        txid: DoubleSha256DigestBE
    ): Future[(BitcoinAddress, CurrencyUnit)] = {
      for {
        rawTx <- client.getRawTransactionRaw(txid)
      } yield {
        val outs = rawTx.outputs.filterNot(_.value == amount)
        val changeAddresses = outs
          .map(out =>
            (
              BitcoinAddress.fromScriptPubKey(out.scriptPubKey, networkParam),
              out.value
            ))
        assert(changeAddresses.size == 1)
        assert(changeAddresses.head._1 != address)
        (changeAddresses.head._1, changeAddresses.head._2)
      }
    }

    for {
      groupingsBefore <- client.listAddressGroupings

      address <- client.getNewAddress

      txid <- BitcoindRpcTestUtil.fundBlockChainTransaction(
        client,
        otherClient,
        address,
        amount
      )

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
        !groupingsBefore.exists(vec => vec.exists(_.address == changeAddress))
      )

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

  it should "generate a bech32m address" in { nodePair =>
    val client = nodePair.node1
    for {
      address <- client.getNewAddress(addressType = AddressType.Bech32m)
    } yield {
      assert(address.isInstanceOf[Bech32mAddress])
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
            .fundBlockChainTransaction(
              client,
              otherClient,
              address,
              Bitcoins(1.5)
            )
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

  it should "be able to list transactions" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
      address <- otherClient.getNewAddress
      txid <-
        BitcoindRpcTestUtil
          .fundBlockChainTransaction(
            client,
            otherClient,
            address,
            Bitcoins(1.5)
          )
      txs <- otherClient.listTransactions()
    } yield {
      assert(txs.nonEmpty)
      assert(txs.exists(_.txid.contains(txid)))
    }
  }

  it should "be able to get the balance" in { nodePair: FixtureParam =>
    val client = nodePair.node1
    for {
      balance <- client.getBalance
      _ <- client.generate(1)
      newBalance <- client.getBalance
    } yield {
      assert(balance.toBigDecimal > 0)
      assert(balance.toBigDecimal < newBalance.toBigDecimal)
    }
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
      FileUtil.removeDirectory(new File(walletFile).toPath)
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
            UInt32.max - UInt32(2)
          )
        val inputs = Vector(input)

        val outputs =
          Map(
            address -> Bitcoins(0.5),
            changeAddress -> Bitcoins(output.amount.toBigDecimal - 0.55)
          )

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
        assert(
          transaction.outputs.contains(
            TransactionOutput(Bitcoins(1), address.scriptPubKey)
          )
        )
      }
  }

  it should "generate the same (low R) signatures as bitcoin-s" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val otherClient = nodePair.node2
      val privKey = ECPrivateKey.freshPrivateKey
      val np = RegTest
      val descriptor = P2WPKHDescriptor(privKey, np)
      val spk = P2WPKHWitnessSPKV0(privKey.publicKey)
      val importedAddress = Bech32Address.fromScriptPubKey(spk, np)
      for {
        fundingTxId <- otherClient.sendToAddress(
          importedAddress,
          Bitcoins(1.01)
        )
        _ <- otherClient.generate(1)
        vout <- otherClient
          .getRawTransactionRaw(fundingTxId)
          .map(
            _.outputs.zipWithIndex
              .find(_._1.scriptPubKey == descriptor.scriptPubKey)
          )
          .map(_.get._2)
        fundingPrevOut = TransactionOutPoint(fundingTxId, vout)
        fundingInput = TransactionInput(
          fundingPrevOut,
          ScriptSignature.empty,
          TransactionConstants.sequence
        )
        address <- otherClient.getNewAddress
        transaction <-
          client
            .createRawTransaction(
              inputs = Vector(fundingInput),
              outputs = Map(address -> Bitcoins.one)
            )
        signedTx <- client
          .signRawTransactionWithKey(transaction, Vector(privKey))
          .map(_.hex)
        _ <- client.broadcastTransaction(signedTx)
        // Validate signature against bitcoin-s generated one
        outPoint = transaction.inputs.head.previousOutput
        prevTx <- client.getRawTransactionRaw(outPoint.txIdBE)
        output = prevTx.outputs(outPoint.vout.toInt)
        _ = BitcoinAddress.fromScriptPubKey(output.scriptPubKey, RegTest)
      } yield {
        val partialSig = BitcoinSigner.signSingle(
          ECSignatureParams(
            P2WPKHV0InputInfo(outPoint, output.value, privKey.publicKey),
            prevTx,
            privKey,
            HashType.sigHashAll
          ),
          transaction,
          isDummySignature = false
        )

        signedTx match {
          case btx: NonWitnessTransaction =>
            assert(
              btx.inputs.head.scriptSignature.signatures.head == partialSig.signature
            )
          case wtx: WitnessTransaction =>
            wtx.witness.head match {
              case p2wpkh: P2WPKHWitnessV0 =>
                assert(p2wpkh.pubKey == partialSig.pubKey)
                assert(p2wpkh.signature == partialSig.signature)
              case _: P2WSHWitnessV0 | EmptyScriptWitness | _: TaprootWitness =>
                fail("Expected P2WPKH")
            }
        }
      }
  }

  it should "be able to set the wallet flag 'avoid_reuse'" in { nodePair =>
    val client = nodePair.node1
    for {
      unspentPre <- client.listUnspent
      result <- client.setWalletFlag(WalletFlag.AvoidReuse, value = true)
      unspentPost <- client.listUnspent
    } yield {
      assert(result.flag_name == "avoid_reuse")
      assert(result.flag_state)
      assert(unspentPre.forall(utxo => utxo.reused.isEmpty))
      assert(unspentPost.forall(utxo => utxo.reused.isDefined))
    }
  }

  it should "create a wallet with a passphrase" in { nodePair =>
    val client = nodePair.node1
    for {
      _ <- client.createWallet("suredbits", passphrase = "stackingsats")
      wallets <- client.listWallets
    } yield {
      assert(wallets.contains("suredbits"))
    }
  }

  it should "check to see if the utxoUpdate input has been updated" in {
    nodePair =>
      val client = nodePair.node1
      val descriptor =
        "pk(0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798)"

      val psbt =
        PSBT.fromBase64(
          "cHNidP8BACoCAAAAAAFAQg8AAAAAABepFG6Rty1Vk+fUOR4v9E6R6YXDFkHwhwAAAAAAAA=="
        )

      for {
        result <- client.utxoUpdatePsbt(psbt, Seq(descriptor))
      } yield {
        assert(result == psbt)
      }
  }

  it should "correct generate to a descriptor" in { nodePair =>
    val client = nodePair.node1
    // 2-of-2 multisig descriptor
    val descriptor =
      "sh(sortedmulti(2,023f720438186fbdfde0c0a403e770a0f32a2d198623a8a982c47b621f8b307640,03ed261094d609d5e02ba6553c2d91e4fd056006ce2fe64aace72b69cb5be3ab9c))#nj9wx7up"
    val numBlocks = 10
    for {
      hashes <- client.generateToDescriptor(numBlocks, descriptor)
    } yield assert(hashes.size == numBlocks)
  }

  it should "correct create multisig and get its descriptor" in { nodePair =>
    val client = nodePair.node1
    val pubKey1 = ECPublicKey.freshPublicKey
    val pubKey2 = ECPublicKey.freshPublicKey

    for {
      multiSigResult <- client.createMultiSig(
        2,
        Vector(pubKey1, pubKey2),
        AddressType.Bech32
      )
    } yield {
      // just validate we are able to receive a sane descriptor
      // no need to check checksum
      assert(
        multiSigResult.descriptor.startsWith(
          s"wsh(multi(2,${pubKey1.hex},${pubKey2.hex}))#"
        )
      )
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
        }
      }
  }
  it should "return a list of wallets" in { nodePair =>
    val client = nodePair.node1
    for {
      _ <- client.createWallet(s"Suredbits-${System.currentTimeMillis()}")
      list <- client.listWalletDir()
    } yield {
      assert(list.wallets.exists(_.name.contains("Suredbits")))
    }
  }

  it should "be able to create a multi sig address" in { case nodePair =>
    val client = nodePair.node1
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val ecPrivKey2 = ECPrivateKey.freshPrivateKey

    val pubKey1 = ecPrivKey1.publicKey
    val pubKey2 = ecPrivKey2.publicKey

    for {
      _ <- client.createMultiSig(
        2,
        Vector(pubKey1, pubKey2),
        AddressType.Bech32
      )
    } yield succeed
  }

  def startClient(client: BitcoindRpcClient): Future[Unit] = {
    BitcoindRpcTestUtil.startServers(Vector(client))
  }

  override def afterAll(): Unit = {
    val stopF = walletClientF.map(BitcoindRpcTestUtil.stopServer)
    Await.result(stopF, 30.seconds)
    super.afterAll()
  }
}
