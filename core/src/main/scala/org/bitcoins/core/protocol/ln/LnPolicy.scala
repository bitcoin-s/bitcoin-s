package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.Int64

sealed abstract class LnPolicy {

  val milliMultiplier: BigDecimal = BigDecimal(0.001)
  val microMultiplier: BigDecimal = BigDecimal(0.000001)
  val nanoMultiplier: BigDecimal = BigDecimal(0.000000001)
  val picoMultiplier: BigDecimal = BigDecimal(0.000000000001)

  val maxPicoBitcoins: BigInt = Int64.max.toBigInt
  val minPicoBitcoins: BigInt = Int64.min.toBigInt
  val maxMilliBitcoins: BigInt = maxPicoBitcoins / (milliMultiplier / picoMultiplier).toBigIntExact().get
  val minMilliBitcoins: BigInt = -(maxPicoBitcoins / (milliMultiplier / picoMultiplier).toBigIntExact().get)
  val maxMicroBitcoins: BigInt = maxPicoBitcoins / (microMultiplier / picoMultiplier).toBigIntExact().get
  val minMicroBitcoins: BigInt = -(maxPicoBitcoins / (microMultiplier / picoMultiplier).toBigIntExact().get)
  val maxNanoBitcoins: BigInt = maxPicoBitcoins / (nanoMultiplier / picoMultiplier).toBigIntExact().get
  val minNanoBitcoins: BigInt = -(maxPicoBitcoins / (nanoMultiplier / picoMultiplier).toBigIntExact().get)
}

object LnPolicy extends LnPolicy