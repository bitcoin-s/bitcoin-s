package org.bitcoins.core.dlc.accounting

object RateOfReturnUtil {

  def prettyPrint(ror: BigDecimal): String = {
    (ror * 100).toString() + "%"
  }
}
