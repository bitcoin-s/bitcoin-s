package org.bitcoins.core.dlc.accounting

import org.bitcoins.core.currency.CurrencyUnit

/** Utility trait for metrics we need to do accounting */
trait PayoutAccounting {
  def myCollateral: CurrencyUnit

  def theirCollateral: CurrencyUnit

  def myPayout: CurrencyUnit

  def theirPayout: CurrencyUnit

  /** Profit and loss for the DLC
    * @see https://www.investopedia.com/terms/p/plstatement.asp
    */
  def pnl: CurrencyUnit = myPayout - myCollateral

  /** Rate of return for the DLC
    * @see https://www.investopedia.com/terms/r/rateofreturn.asp
    */
  def rateOfReturn: BigDecimal = pnl.toBigDecimal / myCollateral.toBigDecimal

  def rorPrettyPrint: String = {
    RateOfReturnUtil.prettyPrint(rateOfReturn)
  }
}
