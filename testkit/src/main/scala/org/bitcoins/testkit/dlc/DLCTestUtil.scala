package org.bitcoins.testkit.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.ContractInfo
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.crypto.{CryptoUtil, Sha256DigestBE}
import scodec.bits.ByteVector

object DLCTestUtil {

  def genOutcomes(size: Int): Vector[Sha256DigestBE] = {
    val strs =
      (0 until size).map(_ => scala.util.Random.nextLong().toString).toVector

    strs.map(str => CryptoUtil.sha256(ByteVector(str.getBytes)).flip)
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

  def genContractInfos(
      outcomeHashes: Vector[Sha256DigestBE],
      totalInput: CurrencyUnit): (ContractInfo, ContractInfo) = {
    val outcomeMap =
      outcomeHashes
        .zip(DLCTestUtil.genValues(outcomeHashes.length, totalInput))
        .toMap

    val otherOutcomeMap = outcomeMap.map {
      case (hash, amt) => (hash, (totalInput - amt).satoshis)
    }

    (ContractInfo(outcomeMap), ContractInfo(otherOutcomeMap))
  }
}
