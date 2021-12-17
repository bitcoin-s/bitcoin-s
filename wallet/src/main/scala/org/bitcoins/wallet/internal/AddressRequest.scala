package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet.db.{AccountDb, AddressDb}
import org.bitcoins.core.hd.HDChainType

import scala.concurrent.Promise

case class AddressRequest(
    accountDb: AccountDb,
    chainType: HDChainType,
    promise: Promise[AddressDb])
