package org.bitcoins.wallet

import org.bitcoins.core.api.dlc.wallet.AnyDLCHDWalletApi
import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.hd.AddressType
import org.bitcoins.core.protocol.BitcoinAddress

import scala.concurrent.Future

/** ScalaMock cannot stub traits with protected methods,
  * so we need to stub them manually.
  */
abstract class MockWalletApi extends AnyDLCHDWalletApi {

  override def getNewChangeAddress(account: AccountDb): Future[BitcoinAddress] =
    stub

  override def getDefaultAccount(): Future[AccountDb] = stub

  override def getDefaultAccountForType(
      addressType: AddressType): Future[AccountDb] = stub

  private def stub[T] =
    Future.failed[T](new RuntimeException("Not implemented"))

}
