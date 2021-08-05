package org.bitcoins.core.dlc.accounting

object RateOfReturnUtil {

  /** @see https://alvinalexander.com/scala/how-to-format-numbers-commas-international-currency-in-scala/ */
  def prettyPrint(ror: BigDecimal): String = {
    toPercentage(ror, 2) + "%"
  }

  /** Convert to default two decimal percentage representation without % label */
  def toPercentage(ror: BigDecimal, decimals: Int = 2): String = {
    s"%1.${decimals}f".format(ror * 100)
  }

}
