package org.bitcoins.core.wallet.fee

import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.protocol.transaction.{
  TransactionInput,
  TransactionOutput
}
import org.bitcoins.core.wallet.builder.OwnedTxData

sealed trait FeeMode {

  def distributeFees(
      inputs: Vector[OwnedTxData[TransactionInput]],
      outputs: Vector[OwnedTxData[TransactionOutput]],
      owners: Int,
      totalFee: Satoshis
  ): Map[Int, Satoshis]
}

object FeeMode {
  case object EqualDistribution extends FeeMode {
    override def distributeFees(
        inputs: Vector[OwnedTxData[TransactionInput]],
        outputs: Vector[OwnedTxData[TransactionOutput]],
        owners: Int,
        totalFee: Satoshis): Map[Int, Satoshis] = {
      val feePerOwner = totalFee.toLong / owners
      val remainder = totalFee.toLong % owners

      (0 until owners).map { owner =>
        val remainderToAdd = if (owner < remainder) 1 else 0
        owner -> Satoshis(feePerOwner + remainderToAdd)
      }.toMap
    }
  }

  private def distributeByMetric(
      metric: Map[Int, Long],
      owners: Int,
      totalFee: Satoshis): Map[Int, Satoshis] = {
    val metricTotal = metric.values.foldLeft(0L)(_ + _)

    val feesByOwner = metric.map {
      case (owner, metricForOwner) =>
        val proportionalBytes = metricForOwner.toDouble / metricTotal
        val fee = Satoshis((proportionalBytes * totalFee.toLong).toLong)
        owner -> fee
    }
    val feePaidSoFar = feesByOwner.values.foldLeft(CurrencyUnits.zero)(_ + _)
    val remainder = totalFee - feePaidSoFar
    val remainderFees = EqualDistribution.distributeFees(Vector.empty,
                                                         Vector.empty,
                                                         owners,
                                                         remainder.satoshis)

    val placeHolderOwnerMap = (0 until owners)
      .filterNot(feesByOwner.keySet.contains)
      .map(_ -> Satoshis.zero)
      .toMap
    (feesByOwner ++ placeHolderOwnerMap).map {
      case (owner, ownerFee) =>
        owner -> (ownerFee + remainderFees(owner)).satoshis
    }
  }

  case object DistributionByInputSize extends FeeMode {
    override def distributeFees(
        inputs: Vector[OwnedTxData[TransactionInput]],
        outputs: Vector[OwnedTxData[TransactionOutput]],
        owners: Int,
        totalFee: Satoshis): Map[Int, Satoshis] = {
      val bytesByOwner = inputs.groupBy(_.owner).map {
        case (owner, ownersData) =>
          val bytes = ownersData.foldLeft(0L)(_ + _.data.bytes.size)
          owner -> bytes
      }

      distributeByMetric(bytesByOwner, owners, totalFee)
    }
  }

  case object DistributionByOutputValue extends FeeMode {
    override def distributeFees(
        inputs: Vector[OwnedTxData[TransactionInput]],
        outputs: Vector[OwnedTxData[TransactionOutput]],
        owners: Int,
        totalFee: Satoshis): Map[Int, Satoshis] = {
      val outputValueByOwner = outputs.groupBy(_.owner).map {
        case (owner, ownersData) =>
          val value = ownersData.foldLeft(0L)(_ + _.data.value.satoshis.toLong)
          owner -> value
      }

      distributeByMetric(outputValueByOwner, owners, totalFee)
    }
  }
}
