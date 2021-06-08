package org.bitcoins.core.dlc.accounting

import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}

/** Similar to [[org.bitcoins.core.dlc.accounting.DLCAccounting]], but
  * represents the entire accounting for the wallet
  */
case class DLCWalletAccounting(
    myCollateral: CurrencyUnit,
    theirCollateral: CurrencyUnit,
    myPayout: CurrencyUnit,
    theirPayout: CurrencyUnit)
    extends PayoutAccounting

object DLCWalletAccounting {

  def fromDLCAccounting(
      accountings: Vector[DLCAccounting]): DLCWalletAccounting = {
    val myCollateral =
      accountings.foldLeft(CurrencyUnits.zero)(_ + _.myCollateral)
    val theirCollateral =
      accountings.foldLeft(CurrencyUnits.zero)(_ + _.theirCollateral)

    val myPayouts = accountings.foldLeft(CurrencyUnits.zero)(_ + _.myPayout)
    val theirPayouts =
      accountings.foldLeft(CurrencyUnits.zero)(_ + _.theirPayout)

    DLCWalletAccounting(myCollateral, theirCollateral, myPayouts, theirPayouts)
  }
}
