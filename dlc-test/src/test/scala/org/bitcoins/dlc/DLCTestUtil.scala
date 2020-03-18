package org.bitcoins.dlc

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}

object DLCTestUtil {

  def genOutcomes(size: Int): Vector[String] = {
    (0 until size).map(_ => scala.util.Random.nextLong().toString).toVector
  }

  def genValues(size: Int, totalAmount: CurrencyUnit): Vector[Satoshis] = {
    val vals = if (size < 2) {
      throw new IllegalArgumentException(
        s"Size must be at least two, got $size")
    } else if (size == 2) {
      Vector(totalAmount.satoshis, Satoshis.zero)
    } else {
      (0 until size - 2).map { _ =>
        Satoshis(scala.util.Random.nextInt(totalAmount.satoshis.toLong.toInt))
      }.toVector :+ totalAmount.satoshis :+ Satoshis.zero
    }

    val valsWithOrder = vals.map(_ -> scala.util.Random.nextDouble())
    valsWithOrder.sortBy(_._2).map(_._1)
  }
}
