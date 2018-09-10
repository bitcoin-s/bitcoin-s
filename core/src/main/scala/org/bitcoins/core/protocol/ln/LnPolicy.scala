package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.Int64

sealed abstract class LnPolicy {

  /**
   * The "amount_msat" field has been artificially limited to a UInt32. This means that the current maximum transaction that can be completed
   * over the lightning network is 4294967295 MilliSatoshi.
   * This is a self imposed limit, and is subject to change.
   * Please see BOLT #2: https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#adding-an-htlc-update_add_htlc
   */
  lazy val maxAmountMSat: MilliBitcoins = MilliBitcoins(4294967295L)

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