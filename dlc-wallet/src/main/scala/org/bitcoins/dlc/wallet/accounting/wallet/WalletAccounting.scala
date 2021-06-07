package org.bitcoins.dlc.wallet.accounting.wallet

import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.dlc.accounting.{DLCAccounting, PayoutAccounting}

/** Similar to [[org.bitcoins.core.dlc.accounting.DLCAccounting]], but
  * represents the entire accounting for the wallet
  */
case class WalletAccounting(
    myCollateral: CurrencyUnit,
    theirCollateral: CurrencyUnit,
    myPayout: CurrencyUnit,
    theirPayout: CurrencyUnit)
    extends PayoutAccounting

object WalletAccounting {

  def fromDLCAccounting(
      accountings: Vector[DLCAccounting]): WalletAccounting = {
    val myCollateral =
      accountings.foldLeft(CurrencyUnits.zero)(_ + _.myCollateral)
    val theirCollateral =
      accountings.foldLeft(CurrencyUnits.zero)(_ + _.theirCollateral)

    val myPayouts = accountings.foldLeft(CurrencyUnits.zero)(_ + _.myPayout)
    val theirPayouts =
      accountings.foldLeft(CurrencyUnits.zero)(_ + _.theirPayout)

    WalletAccounting(myCollateral, theirCollateral, myPayouts, theirPayouts)
  }
}
