package org.bitcoins.core.gen.ln

import org.bitcoins.core.gen.{ CryptoGenerators, NumberGenerator }
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.ln.ShortChannelId
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.fee.{ FeeBaseMSat, FeeProportionalMillionths }
import org.bitcoins.core.protocol.ln.routing.LnRoute
import org.scalacheck.Gen

trait LnRouteGen {
  def shortChannelId: Gen[ShortChannelId] = {
    NumberGenerator.uInt64s.map { u64 =>
      ShortChannelId(u64)
    }
  }

  def feeBaseMSat: Gen[FeeBaseMSat] = for {
    //note that the feebase msat is only 4 bytes
    u32 <- NumberGenerator.uInt32s
  } yield {
    val ms = MilliSatoshis(u32.toBigInt)
    FeeBaseMSat(ms)
  }

  def feeProportionalMillionths: Gen[FeeProportionalMillionths] = for {
    fee <- NumberGenerator.uInt32s
  } yield FeeProportionalMillionths(fee)

  def route: Gen[LnRoute] = for {
    pubKey <- CryptoGenerators.publicKey
    id <- shortChannelId
    baseFee <- feeBaseMSat
    feeProp <- feeProportionalMillionths
    cltvExpiryDelta <- NumberGenerator.positiveShort
  } yield LnRoute(
    pubkey = pubKey,
    shortChannelID = id,
    feeBaseMsat = baseFee,
    feePropMilli = feeProp,
    cltvExpiryDelta = cltvExpiryDelta)

  def routes: Gen[Vector[LnRoute]] = {
    Gen.choose(1, 5)
      .flatMap(n => Gen.listOfN(n, route).map(_.toVector))
  }
}

object LnRouteGen extends LnRouteGen