package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.ln.currency.{LnMultiplier, MilliSatoshis}

sealed abstract class LnPolicy {

  /** The "amount_msat" field has been artificially limited to a UInt32. This means that the current maximum transaction that can be completed
    * over the lightning network is 4294967295 MilliSatoshi.
    * This is a self imposed limit, and is subject to change.
    * Please see [[https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#adding-an-htlc-update_add_htlc BOLT2]]
    * for more info.
    */
  val maxAmountMSat: MilliSatoshis = MilliSatoshis(4294967295L)

  val maxPicoBitcoins: BigInt = Int64.max.toBigInt
  val minPicoBitcoins: BigInt = Int64.min.toBigInt

  val maxMilliBitcoins: BigInt = {
    calc(LnMultiplier.Milli)
  }

  val minMilliBitcoins: BigInt = -maxMilliBitcoins

  val maxMicroBitcoins: BigInt = {
    calc(LnMultiplier.Micro)
  }
  val minMicroBitcoins: BigInt = -maxMicroBitcoins

  val maxNanoBitcoins: BigInt = {
    calc(LnMultiplier.Nano)
  }

  val minNanoBitcoins: BigInt = -maxNanoBitcoins

  private def calc(mul: LnMultiplier): BigInt = {
    maxPicoBitcoins /
      (mul.multiplier / LnMultiplier.Pico.multiplier).toBigIntExact.get
  }

  val DEFAULT_LN_P2P_PORT = 9735

  val DEFAULT_ECLAIR_API_PORT = 8080

}

object LnPolicy extends LnPolicy
