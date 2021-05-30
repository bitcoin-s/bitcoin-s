package org.bitcoins.core.dlc.accounting

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.crypto.Sha256Digest

case class DlcAccounting(
    dlcId: Sha256Digest,
    myCollateral: CurrencyUnit,
    theirCollateral: CurrencyUnit,
    myPayoutAddress: BitcoinAddress,
    theirPayoutAddress: BitcoinAddress,
    myPayout: CurrencyUnit,
    theirPayout: CurrencyUnit) {
  val pnl: CurrencyUnit = myPayout - myCollateral
}
