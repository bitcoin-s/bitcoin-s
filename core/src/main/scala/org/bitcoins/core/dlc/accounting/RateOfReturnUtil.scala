package org.bitcoins.core.dlc.accounting

object RateOfReturnUtil {

  /** @see https://alvinalexander.com/scala/how-to-format-numbers-commas-international-currency-in-scala/ */
  def prettyPrint(ror: BigDecimal): String = {
    val percent = ror * 100
    f"${percent}%1.2f" + "%"
  }
}
