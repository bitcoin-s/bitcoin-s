package org.bitcoins.server.grpc

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

  it must "getutxos" in { case clientServer =>
    val client = clientServer.client
    client.getUtxos(GetUtxosRequest()).map { response =>
      assert(response.utxos.isEmpty)
    }
  }

  it must "getaddresses" in { case clientServer =>
    val client = clientServer.client
    client.getAddresses(GetAddressesRequest()).map { response =>
      assert(response.addresses.isEmpty)
    }
  }

  it must "getaddresslabels" in { case clientServer =>
    val client = clientServer.client
    client.getAddressLabels(GetAddressLabelsRequest()).map { response =>
      assert(response.addressLabels.isEmpty)
    }
  }
}
