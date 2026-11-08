package org.bitcoins.server.grpc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.hd.HDPurpose
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.core.api.commons.ArgumentSource
import org.bitcoins.core.crypto.{ExtKeyVersion, ExtPrivateKey}
import org.bitcoins.keymanager.WalletStorage
import org.bitcoins.testkit.fixtures.ServerGrpcFixture
import io.grpc.{Status, StatusRuntimeException}
import org.scalatest.FutureOutcome

import scala.jdk.CollectionConverters._

class WalletGrpcRoutesTest extends ServerGrpcFixture {
  override type GrpcClient = WalletRoutesClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withWalletRoutesClient(test)
  }

  behavior of "WalletGrpcRoutes"

  // ── Pre-existing tests ─────────────────────────────────────────────────────

  it must "isempty" in { case clientServer =>
    val client = clientServer.client
    client.isEmpty(IsEmptyRequest()).map { response =>
      assert(response.empty)
    }
  }

  it must "getbalances" in { case clientServer =>
    val client = clientServer.client
    client.getBalances(GetBalancesRequest(isSats = true)).map { response =>
      assert(response.confirmed == 0.0)
      assert(response.unconfirmed == 0.0)
      assert(response.reserved == 0.0)
      assert(response.total == 0.0)
    }
  }

  it must "getbalance" in { case clientServer =>
    val client = clientServer.client
    val wallet = clientServer.walletApi

    for {
      satsResp <- client.getBalance(GetBalanceRequest(isSats = true))
      btcResp <- client.getBalance(GetBalanceRequest(isSats = false))
      expected <- wallet.getBalance()
    } yield {
      assert(satsResp.balance == expected.satoshis.toLong.toDouble)
      assert(
        btcResp.balance == Bitcoins(expected.satoshis).toBigDecimal.toDouble)
    }
  }

  it must "getconfirmedbalance" in { case clientServer =>
    val client = clientServer.client
    val wallet = clientServer.walletApi

    for {
      satsResp <- client.getConfirmedBalance(GetBalanceRequest(isSats = true))
      btcResp <- client.getConfirmedBalance(GetBalanceRequest(isSats = false))
      expected <- wallet.getConfirmedBalance()
    } yield {
      assert(satsResp.balance == expected.satoshis.toLong.toDouble)
      assert(
        btcResp.balance == Bitcoins(expected.satoshis).toBigDecimal.toDouble)
    }
  }

  it must "getunconfirmedbalance" in { case clientServer =>
    val client = clientServer.client
    val wallet = clientServer.walletApi

    for {
      satsResp <-
        client.getUnconfirmedBalance(GetBalanceRequest(isSats = true))
      btcResp <-
        client.getUnconfirmedBalance(GetBalanceRequest(isSats = false))
      expected <- wallet.getUnconfirmedBalance()
    } yield {
      assert(satsResp.balance == expected.satoshis.toLong.toDouble)
      assert(
        btcResp.balance == Bitcoins(expected.satoshis).toBigDecimal.toDouble)
    }
  }

  it must "getutxos" in { case clientServer =>
    val client = clientServer.client
    client.getUtxos(GetUtxosRequest()).map { response =>
      assert(response.utxos.isEmpty)
    }
  }

  it must "getreservedutxos" in { case clientServer =>
    val client = clientServer.client
    val wallet = clientServer.walletApi

    for {
      response <- client.getReservedUtxos(GetReservedUtxosRequest())
      expected <- wallet.utxoHandling.getUtxos(TxoState.Reserved)
    } yield {
      val expectedTuples = expected.map(utxo =>
        (utxo.txid.hex,
         utxo.outPoint.vout.toLong,
         utxo.output.value.satoshis.toLong))
      val actualTuples =
        response.utxos.map(utxo => (utxo.txid, utxo.vout, utxo.valueSats))

      assert(actualTuples.toSet == expectedTuples.toSet)
    }
  }

  it must "getaddresses" in { case clientServer =>
    val client = clientServer.client
    client.getAddresses(GetAddressesRequest()).map { response =>
      assert(response.addresses.isEmpty)
    }
  }

  it must "getspentaddresses" in { case clientServer =>
    val client = clientServer.client
    val wallet = clientServer.walletApi

    for {
      response <- client.getSpentAddresses(GetSpentAddressesRequest())
      expected <- wallet.addressHandling.getSpentAddresses()
    } yield {
      assert(response.addresses.toSet == expected.map(_.address.value).toSet)
    }
  }

  it must "getfundedaddresses" in { case clientServer =>
    val client = clientServer.client
    val wallet = clientServer.walletApi

    for {
      response <- client.getFundedAddresses(GetFundedAddressesRequest())
      expected <- wallet.addressHandling.getFundedAddresses()
    } yield {
      val expectedTuples =
        expected.map { case (addr, value) =>
          (addr.address.value, value.satoshis.toLong)
        }
      val actualTuples =
        response.fundedAddresses.map(addr => (addr.address, addr.valueSats))

      assert(actualTuples.toSet == expectedTuples.toSet)
    }
  }

  it must "getunusedaddresses" in { case clientServer =>
    val client = clientServer.client
    val wallet = clientServer.walletApi

    for {
      response <- client.getUnusedAddresses(GetUnusedAddressesRequest())
      expected <- wallet.addressHandling.getUnusedAddresses()
    } yield {
      assert(response.addresses.toSet == expected.map(_.address.value).toSet)
    }
  }

  it must "getaccounts" in { case clientServer =>
    val client = clientServer.client
    val wallet = clientServer.walletApi

    for {
      response <- client.getAccounts(GetAccountsRequest())
      expected <- wallet.accountHandling.getAccounts()
    } yield {
      assert(response.xpubs.toSet == expected.map(_.xpub.toString).toSet)
    }
  }

  it must "getaddresslabels" in { case clientServer =>
    val client = clientServer.client
    client.getAddressLabels(GetAddressLabelsRequest()).map { response =>
      assert(response.addressLabels.isEmpty)
    }
  }

  // ── New endpoint tests ─────────────────────────────────────────────────────

  it must "getwalletinfo" in { case clientServer =>
    val client = clientServer.client
    val wallet = clientServer.walletApi

    for {
      response <- client.getWalletInfo(GetWalletInfoRequest())
      expected <- wallet.getInfo()
    } yield {
      assert(response.walletName == expected.walletName)
      assert(response.rootXpub == expected.rootXpub.toString)
      assert(response.xpub == expected.xpub.toString)
      assert(response.hdAccount == expected.hdAccount.toString)
      assert(response.height == expected.height)
      assert(response.blockHash == expected.blockHash.hex)
      assert(response.rescan == expected.rescan)
      assert(response.imported == expected.imported)
    }
  }

  it must "getnewaddress" in { case clientServer =>
    val client = clientServer.client
    client.getNewAddress(GetNewAddressRequest()).map { response =>
      assert(response.address.nonEmpty)
    }
  }

  it must "getnewaddress with label" in { case clientServer =>
    val client = clientServer.client
    client
      .getNewAddress(GetNewAddressRequest(label = Some("test-label")))
      .map { response =>
        assert(response.address.nonEmpty)
      }
  }

  it must "gettransaction returns empty for unknown txid" in {
    case clientServer =>
      val client = clientServer.client
      // Use a zeroed txid which is unlikely to exist in the wallet
      val zeroTxId = "0" * 64
      client.getTransaction(GetTransactionRequest(txid = zeroTxId)).map {
        response =>
          assert(response.txHex.isEmpty)
      }
  }

  it must "getaddresstags returns empty tags for fresh address" in {
    case clientServer =>
      val client = clientServer.client
      for {
        addrResp <- client.getNewAddress(GetNewAddressRequest())
        tagsResp <- client.getAddressTags(
          GetAddressTagsRequest(address = addrResp.address))
      } yield {
        assert(tagsResp.tags.isEmpty)
      }
  }

  it must "getaddresslabel returns empty labels for fresh address" in {
    case clientServer =>
      val client = clientServer.client
      for {
        addrResp <- client.getNewAddress(GetNewAddressRequest())
        labelResp <- client.getAddressLabel(
          GetAddressLabelRequest(address = addrResp.address))
      } yield {
        assert(labelResp.labels.isEmpty)
      }
  }

  it must "labeladdress then getaddresstags" in { case clientServer =>
    val client = clientServer.client
    val label = "my-test-label"
    for {
      addrResp <- client.getNewAddress(GetNewAddressRequest())
      labelResult <- client.labelAddress(
        LabelAddressRequest(address = addrResp.address, label = label))
      tagsResp <- client.getAddressTags(
        GetAddressTagsRequest(address = addrResp.address))
    } yield {
      assert(labelResult.message.contains(label))
      assert(tagsResp.tags.contains(label))
    }
  }

  it must "labeladdress then getaddresslabel" in { case clientServer =>
    val client = clientServer.client
    val label = "label-for-label-test"
    for {
      addrResp <- client.getNewAddress(GetNewAddressRequest())
      _ <- client.labelAddress(
        LabelAddressRequest(address = addrResp.address, label = label))
      labelResp <- client.getAddressLabel(
        GetAddressLabelRequest(address = addrResp.address))
    } yield {
      assert(labelResp.labels.contains(label))
    }
  }

  it must "dropaddresslabel" in { case clientServer =>
    val client = clientServer.client
    val label = "drop-me"
    for {
      addrResp <- client.getNewAddress(GetNewAddressRequest())
      _ <- client.labelAddress(
        LabelAddressRequest(address = addrResp.address, label = label))
      dropResp <- client.dropAddressLabel(
        DropAddressLabelRequest(address = addrResp.address, label = label))
      tagsAfter <- client.getAddressTags(
        GetAddressTagsRequest(address = addrResp.address))
    } yield {
      assert(dropResp.message.contains("dropped"))
      assert(!tagsAfter.tags.contains(label))
    }
  }

  it must "dropaddresslabels" in { case clientServer =>
    val client = clientServer.client
    for {
      addrResp <- client.getNewAddress(GetNewAddressRequest())
      _ <- client.labelAddress(
        LabelAddressRequest(address = addrResp.address, label = "lbl1"))
      _ <- client.labelAddress(
        LabelAddressRequest(address = addrResp.address, label = "lbl2"))
      dropResp <- client.dropAddressLabels(
        DropAddressLabelsRequest(address = addrResp.address))
      tagsAfter <- client.getAddressTags(
        GetAddressTagsRequest(address = addrResp.address))
    } yield {
      assert(dropResp.message.nonEmpty)
      assert(tagsAfter.tags.isEmpty)
    }
  }

  it must "lockunspent unlock with empty list succeeds on empty wallet" in {
    case clientServer =>
      val client = clientServer.client
      // Unlock all (empty list means all); on an empty wallet this is a no-op
      client
        .lockUnspent(LockUnspentRequest(unlock = true, outpoints = Seq.empty))
        .map { response =>
          // No UTXOs to unlock → success is false (none were actually unlocked)
          assert(!response.success)
        }
  }

  it must "getaddressinfo for a known address" in { case clientServer =>
    val client = clientServer.client
    for {
      addrResp <- client.getNewAddress(GetNewAddressRequest())
      infoResp <- client.getAddressInfo(
        GetAddressInfoRequest(address = addrResp.address))
    } yield {
      assert(infoResp.pubkey.isDefined)
      assert(infoResp.hdPath.isDefined)
      assert(infoResp.pubkey.exists(_.nonEmpty))
    }
  }

  it must "getaddressinfo for unknown address returns no pubkey" in {
    case clientServer =>
      val client = clientServer.client
      val p2wpkh = P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)
      val foreignAddress =
        BitcoinAddress.fromScriptPubKey(p2wpkh, RegTest)
      client
        .getAddressInfo(
          GetAddressInfoRequest(address = foreignAddress.toString))
        .map { infoResp =>
          assert(infoResp.pubkey.isEmpty)
          assert(infoResp.hdPath.isEmpty)
        }
  }

  it must "createnewaccount with segwit purpose" in { case clientServer =>
    val client = clientServer.client
    val wallet = clientServer.walletApi

    for {
      accountsBefore <- wallet.accountHandling.getAccounts()
      response <- client.createNewAccount(
        CreateNewAccountRequest(purpose = HDPurpose.SegWit.toString))
    } yield {
      // The response should contain all accounts, including the new one
      assert(response.xpubs.size >= accountsBefore.size)
      assert(response.xpubs.nonEmpty)
    }
  }

  it must "rescan returns a message" in { case clientServer =>
    val client = clientServer.client
    client
      .rescan(
        RescanRequest(ignoreCreationTime = true,
                      batchSize = Some(100),
                      startBlock = None,
                      endBlock = None))
      .map { response =>
        assert(
          response.message == "Rescan started." ||
            response.message == "Rescan done.")
      }
  }

  it must "estimatefee returns a fee rate" in { case clientServer =>
    val client = clientServer.client
    client.estimateFee(EstimateFeeRequest()).map { response =>
      // Any double value is acceptable; -1 is returned when the fee provider fails
      assert(response.satsPerVbyte >= -1.0)
    }
  }

  // ── Key-manager / wallet-lifecycle endpoints ───────────────────────────────

  private val mnemonic =
    "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"

  private def uniqueName(prefix: String): String =
    prefix + scala.util.Random.alphanumeric.take(8).mkString.toLowerCase

  private def assertGrpcStatus(
      f: scala.concurrent.Future[?],
      code: Status.Code) = {
    f.failed.map {
      case e: StatusRuntimeException =>
        assert(e.getStatus.getCode == code)
      case other =>
        fail(s"Expected StatusRuntimeException with $code, got $other")
    }
  }

  it must "importseed then exportseed round trips the mnemonic" in {
    case clientServer =>
      val client = clientServer.client
      val name = uniqueName("imp")
      for {
        imported <- client.importSeed(
          ImportSeedRequest(walletName = Some(name), mnemonic = mnemonic))
        exported <- client.exportSeed(
          ExportSeedRequest(walletName = Some(name)))
      } yield {
        assert(imported.success)
        assert(exported.mnemonic == mnemonic)
      }
  }

  it must "importseed does not overwrite an existing seed" in {
    case clientServer =>
      val client = clientServer.client
      val name = uniqueName("dup")
      val req = ImportSeedRequest(walletName = Some(name), mnemonic = mnemonic)
      for {
        first <- client.importSeed(req)
        second <- client.importSeed(req)
      } yield {
        assert(first.success)
        assert(!second.success)
      }
  }

  it must "importseed with a password requires that password to export" in {
    case clientServer =>
      val client = clientServer.client
      val name = uniqueName("enc")
      val pw = "topsecret"
      for {
        _ <- client.importSeed(
          ImportSeedRequest(walletName = Some(name),
                            mnemonic = mnemonic,
                            password = Some(pw)))
        exported <- client.exportSeed(
          ExportSeedRequest(walletName = Some(name), password = Some(pw)))
        wrong <- assertGrpcStatus(
          client.exportSeed(
            ExportSeedRequest(walletName = Some(name),
                              password = Some("wrong"))),
          Status.Code.FAILED_PRECONDITION)
      } yield {
        assert(exported.mnemonic == mnemonic)
        wrong
      }
  }

  it must "importseed rejects an invalid mnemonic and wallet name" in {
    case clientServer =>
      val client = clientServer.client
      for {
        _ <- assertGrpcStatus(
          client.importSeed(
            ImportSeedRequest(walletName = Some(uniqueName("bad")),
                              mnemonic = "not a real mnemonic")),
          Status.Code.INVALID_ARGUMENT)
        r <- assertGrpcStatus(
          client.importSeed(
            ImportSeedRequest(walletName = Some("bad name!"),
                              mnemonic = mnemonic)),
          Status.Code.INVALID_ARGUMENT)
      } yield r
  }

  it must "markseedasbackedup sets the backup time" in { case clientServer =>
    val client = clientServer.client
    val name = uniqueName("bkp")
    for {
      _ <- client.importSeed(
        ImportSeedRequest(walletName = Some(name), mnemonic = mnemonic))
      before <- client.getSeedBackupTime(
        GetSeedBackupTimeRequest(walletName = Some(name)))
      marked <- client.markSeedAsBackedUp(
        MarkSeedAsBackedUpRequest(walletName = Some(name)))
      after <- client.getSeedBackupTime(
        GetSeedBackupTimeRequest(walletName = Some(name)))
      // once backed up the mnemonic can no longer be exported
      r <- assertGrpcStatus(
        client.exportSeed(ExportSeedRequest(walletName = Some(name))),
        Status.Code.FAILED_PRECONDITION)
    } yield {
      assert(before.backupEpochSeconds.isEmpty)
      assert(marked.message == "ok")
      assert(after.backupEpochSeconds.exists(_ > 0))
      r
    }
  }

  it must "exportseed fails for a wallet that does not exist" in {
    case clientServer =>
      assertGrpcStatus(
        clientServer.client.exportSeed(
          ExportSeedRequest(walletName = Some(uniqueName("missing")))),
        Status.Code.FAILED_PRECONDITION)
  }

  it must "importxprv writes a seed that can be read back" in {
    case clientServer =>
      val client = clientServer.client
      val name = uniqueName("xprv")
      val xprv = ExtPrivateKey.freshRootKey(ExtKeyVersion.LegacyMainNetPriv)
      for {
        _ <- client.importXprv(
          ImportXprvRequest(walletName = Some(name),
                            xprv = xprv.toStringSensitive))
        backup <- client.getSeedBackupTime(
          GetSeedBackupTimeRequest(walletName = Some(name)))
        wallets <- client.listWallets(ListWalletsRequest())
      } yield {
        assert(backup.backupEpochSeconds.isEmpty)
        assert(wallets.walletNames.contains(name))
      }
  }

  it must "importxprv rejects an invalid xprv" in { case clientServer =>
    assertGrpcStatus(clientServer.client.importXprv(
                       ImportXprvRequest(walletName = Some(uniqueName("badx")),
                                         xprv = "not-an-xprv")),
                     Status.Code.INVALID_ARGUMENT)
  }

  it must "listwallets returns imported wallet names" in { case clientServer =>
    val client = clientServer.client
    val a = uniqueName("lsa")
    val b = uniqueName("lsb")
    for {
      _ <- client.importSeed(
        ImportSeedRequest(walletName = Some(a), mnemonic = mnemonic))
      _ <- client.importSeed(
        ImportSeedRequest(walletName = Some(b), mnemonic = mnemonic))
      wallets <- client.listWallets(ListWalletsRequest())
    } yield {
      assert(wallets.walletNames.contains(a))
      assert(wallets.walletNames.contains(b))
    }
  }

  it must "keymanagerpassphraseset and keymanagerpassphrasechange re-encrypt the seed" in {
    case clientServer =>
      val client = clientServer.client
      val lifecycle = clientServer.lifecycleApi
      val name = uniqueName("pw")
      // point "the current wallet" at a throwaway seed instead of the fixture's
      lifecycle.kmConfOverrideOpt = Some(
        lifecycle.kmConf.copy(walletNameOverride =
          Some(ArgumentSource.RpcArgument(name))))
      val seedPath = lifecycle.kmConf.seedPath
      for {
        _ <- client.importSeed(
          ImportSeedRequest(walletName = Some(name), mnemonic = mnemonic))
        _ <- client.keyManagerPassphraseSet(
          KeyManagerPassphraseSetRequest(password = "first"))
        exported1 <- client.exportSeed(
          ExportSeedRequest(walletName = Some(name), password = Some("first")))
        _ <- client.keyManagerPassphraseChange(
          KeyManagerPassphraseChangeRequest(oldPassword = "first",
                                            newPassword = "second"))
        exported2 <- client.exportSeed(
          ExportSeedRequest(walletName = Some(name), password = Some("second")))
        oldFails <- assertGrpcStatus(
          client.exportSeed(
            ExportSeedRequest(walletName = Some(name),
                              password = Some("first"))),
          Status.Code.FAILED_PRECONDITION)
      } yield {
        assert(WalletStorage.seedExists(seedPath))
        assert(exported1.mnemonic == mnemonic)
        assert(exported2.mnemonic == mnemonic)
        oldFails
      }
  }

  it must "loadwallet delegates to the wallet loader" in { case clientServer =>
    val client = clientServer.client
    val name = uniqueName("load")
    client
      .loadWallet(
        LoadWalletRequest(walletName = Some(name),
                          aesPassword = Some("password")))
      .map { response =>
        assert(response.walletName == name)
        val requests = clientServer.lifecycleApi.loadRequests.asScala.toVector
        assert(requests.map(_._1) == Vector(Some(name)))
        assert(requests.head._2.exists(_.toStringSensitive == "password"))
      }
  }

  it must "loadwallet rejects an invalid wallet name" in { case clientServer =>
    assertGrpcStatus(clientServer.client.loadWallet(
                       LoadWalletRequest(walletName = Some("bad name!"))),
                     Status.Code.INVALID_ARGUMENT)
  }
}
