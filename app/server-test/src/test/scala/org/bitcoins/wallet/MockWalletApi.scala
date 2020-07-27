package org.bitcoins.wallet

import org.bitcoins.core.hd.AddressType
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.wallet.api.HDWalletApi
import org.bitcoins.wallet.models.AccountDb

import scala.concurrent.Future

/**
  * ScalaMock cannot stub traits with protected methods,
  * so we need to stub them manually.
  */
abstract class MockWalletApi extends HDWalletApi {

  override protected[wallet] def getNewChangeAddress(
      account: AccountDb): Future[BitcoinAddress] = stub

  override protected[wallet] def getDefaultAccount(): Future[AccountDb] = stub

  override protected[wallet] def getDefaultAccountForType(
      addressType: AddressType): Future[AccountDb] = stub

  private def stub[T] =
    Future.failed[T](new RuntimeException("Not implemented"))

}
