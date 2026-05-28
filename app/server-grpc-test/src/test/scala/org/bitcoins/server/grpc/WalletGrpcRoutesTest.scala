package org.bitcoins.server.grpc

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.testkit.fixtures.ServerGrpcFixture
import org.scalatest.FutureOutcome

class WalletGrpcRoutesTest extends ServerGrpcFixture {
  override type GrpcClient = WalletRoutesClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withWalletRoutesClient(test)
  }

  behavior of "WalletGrpcRoutes"

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
}
