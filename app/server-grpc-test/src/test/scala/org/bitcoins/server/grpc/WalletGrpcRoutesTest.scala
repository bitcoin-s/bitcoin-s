package org.bitcoins.server.grpc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.hd.HDPurpose
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.testkit.fixtures.ServerGrpcFixture
import org.scalatest.FutureOutcome

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
}
