package org.bitcoins.core.protocol.ln.currency

/** Used by [[org.bitcoins.core.protocol.ln.currency.LnCurrencyUnit LnCurrencyUnit]]
  * to scale between values
  */
sealed abstract class LnMultiplier {
  val multiplier: BigDecimal
}

object LnMultiplier {
  case object Milli extends LnMultiplier {
    val multiplier: BigDecimal = BigDecimal(0.001)
  }

  case object Micro extends LnMultiplier {
    val multiplier: BigDecimal = BigDecimal(0.000001)
  }

  case object Nano extends LnMultiplier {
    override val multiplier: BigDecimal = BigDecimal(0.000000001)
  }

  case object Pico extends LnMultiplier {
    override val multiplier: BigDecimal = BigDecimal(0.000000000001)
  }
}
