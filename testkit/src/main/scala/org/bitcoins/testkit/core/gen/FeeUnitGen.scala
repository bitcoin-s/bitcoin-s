package org.bitcoins.testkit.core.gen

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.wallet.fee._
import org.scalacheck.Gen

abstract class FeeUnitGen {

  def satsPerByte: Gen[SatoshisPerByte] = {
    for {
      curr <- realisticFee
    } yield SatoshisPerByte(curr)
  }

  def satsPerKiloByte: Gen[SatoshisPerKiloByte] = {
    for {
      curr <- realisticFee
    } yield SatoshisPerKiloByte(curr * 1000)
  }

  def satsPerKiloWeight: Gen[SatoshisPerKW] = {
    for {
      curr <- realisticFee
    } yield SatoshisPerKW(curr * 1000)
  }

  def satsPerVirtualByte: Gen[SatoshisPerVirtualByte] = {
    for {
      curr <- realisticFee
    } yield SatoshisPerVirtualByte(curr)
  }

  /** Choose between different sets of Satoshi sizes so we get a better distribution */
  private def realisticFee: Gen[Satoshis] = {
    Gen.oneOf(lowFee, highFee, exorbitantFee)
  }

  /** Max fee between 101 - 100 sats. So we can have
    * 1. 100 sats/byte
    * 2. 100 sats/vbyte
    * 3. 100 sats/kb
    */
  private def lowFee: Gen[Satoshis] = {
    Gen
      .choose(0, 100)
      .map(Satoshis(_))
  }

  /** Max fee between 0 - 300 sats. So we can have
    * 1. 300 sats/byte
    * 2. 300 sats/vbyte
    * 3. 300 sats/kb
    */
  private def highFee: Gen[Satoshis] = {
    Gen
      .choose(101, 300)
      .map(Satoshis(_))
  }

  /** Max fee between 301 - 1,000 sats. So we can have
    * 1. 1,000 sats/byte
    * 2. 1,000 sats/vbyte
    * 3. 1,000 sats/kb
    *
    * Anything higher can cause a bitcoind to reject the transaction
    * for paying too high a fee
    */
  private def exorbitantFee: Gen[Satoshis] = {
    Gen
      .choose(301, 1000)
      .map(Satoshis(_))
  }

  def feeUnit: Gen[FeeUnit] =
    Gen.oneOf(satsPerByte,
              satsPerKiloByte,
              satsPerVirtualByte,
              satsPerKiloWeight)

  /** Generates a FeeUnit based on the maxFee allowed for a transaction */
  def feeUnit(maxFee: Long): Gen[FeeUnit] = {
    Gen.choose(0L, maxFee / 10000L).map { n =>
      SatoshisPerKiloByte(Satoshis(n))
    }
  }
}

object FeeUnitGen extends FeeUnitGen
