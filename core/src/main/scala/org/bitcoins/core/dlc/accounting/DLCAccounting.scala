package org.bitcoins.core.dlc.accounting

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.crypto.Sha256Digest

case class DLCAccounting(
    dlcId: Sha256Digest,
    myCollateral: CurrencyUnit,
    theirCollateral: CurrencyUnit,
    myPayout: CurrencyUnit,
    theirPayout: CurrencyUnit)
    extends PayoutAccounting
