package org.bitcoins.core.protocol.dlc.models

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.SeqWrapper

/** Fully determines a set of payouts for a DLC.
  * These payouts are normally from the point of
  * view of the offerer, from which the accepter's
  * payouts can be determined by subtracting from totalCollateral.
  *
  * Payouts above totalCollateral may be subject to change
  * as totalCollateral does not exist in a ContractDescriptor,
  * which is reusable between DLCs.
  * @see https://github.com/discreetlogcontracts/dlcspecs/blob/a8876ed28ed33d5f7d5104f01aa2a8d80d128460/Messaging.md#version-1-contract_descriptor
  */
sealed trait ContractDescriptor
    extends DLCSpecTypeSerializable[ContractDescriptorTLV] {

  /** Returns the counter-party's ContractDescriptor corresponding to this one.
    *
    * WARNING: this(outcome) + flip(TC)(outcome) is not guaranteed to equal TC.
    * As such, this should not be used to generate pairs of ContractInfos and
    * should only be used to replace a ContractInfo with another one of the flipped
    * perspective.
    * An example case is for MultiNonceContractInfo where flipping the interpolation points
    * could lead to an off-by-one after rounding so that the sum above gives TC-1.
    * In this example, only the offerer's ContractInfo should be used.
    */
  def flip(totalCollateral: Satoshis): ContractDescriptor
}

object ContractDescriptor
    extends DLCSpecTypeDeserializable[
      ContractDescriptorTLV,
      ContractDescriptor](ContractDescriptorTLV) {

  val empty: ContractDescriptor = EnumContractDescriptor(
    Vector(EnumOutcome("") -> Satoshis.zero))

  override def fromSubType(tlv: ContractDescriptorTLV): ContractDescriptor = {
    tlv match {
      case tlv: ContractDescriptorV0TLV =>
        EnumContractDescriptor.fromSubType(tlv)
      case tlv: ContractDescriptorV1TLV =>
        NumericContractDescriptor.fromSubType(tlv)
    }
  }
}

/** The ContractDescriptor for enumerated outcome DLCs */
case class EnumContractDescriptor(
    outcomeValueMap: Vector[(EnumOutcome, Satoshis)])
    extends ContractDescriptor
    with DLCSpecTypeSerializable[ContractDescriptorV0TLV]
    with SeqWrapper[(EnumOutcome, Satoshis)] {

  require(outcomeValueMap.nonEmpty, "Cannot give an empty set of outcomes")

  override def wrapped: Vector[(EnumOutcome, Satoshis)] = outcomeValueMap

  def keys: Vector[EnumOutcome] = outcomeValueMap.map(_._1)

  def values: Vector[Satoshis] = outcomeValueMap.map(_._2)

  override def toSubType: ContractDescriptorV0TLV =
    ContractDescriptorV0TLV(outcomeValueMap.map { case (outcome, amt) =>
                              outcome.outcome -> amt
                            },
                            DLCSerializationVersion.current)

  override def flip(totalCollateral: Satoshis): EnumContractDescriptor = {
    EnumContractDescriptor(outcomeValueMap.map { case (hash, amt) =>
      (hash, (totalCollateral - amt).satoshis)
    })
  }
}

object EnumContractDescriptor
    extends DLCSpecTypeDeserializable[
      ContractDescriptorV0TLV,
      EnumContractDescriptor](ContractDescriptorV0TLV) {

  def fromStringVec(vec: Vector[(String, Satoshis)]): EnumContractDescriptor = {
    EnumContractDescriptor(vec.map { case (str, amt) =>
      EnumOutcome(str) -> amt
    })
  }

  override def fromSubType(
      tlv: ContractDescriptorV0TLV): EnumContractDescriptor = {
    fromStringVec(tlv.outcomes)
  }
}

/** The ContractDescriptor for enumerated outcome DLCs
  *
  * Contains a deterministically compressed set of outcomes computed by
  * interpolation of a given payout curve.
  */
case class NumericContractDescriptor(
    outcomeValueFunc: DLCPayoutCurve,
    numDigits: Int,
    roundingIntervals: RoundingIntervals)
    extends ContractDescriptor
    with DLCSpecTypeSerializable[ContractDescriptorV1TLV] {

  private val minValue: Long = 0L
  private val maxValue: Long = (Math.pow(2, numDigits) - 1).toLong

  require(
    outcomeValueFunc.endpoints.head.outcome == 0,
    s"Payout curve must start with its minimum value, $minValue, got ${outcomeValueFunc.endpoints.head.outcome}. " +
      s"You must define the payout curve from $minValue - $maxValue"
  )

  require(
    outcomeValueFunc.endpoints.last.outcome == maxValue,
    s"Payout curve must end with its maximum value, $maxValue, got ${outcomeValueFunc.endpoints.last.outcome}. " +
      s"You must define the payout curve from $minValue - $maxValue"
  )

  override def flip(totalCollateral: Satoshis): NumericContractDescriptor = {
    NumericContractDescriptor(
      outcomeValueFunc.flip(totalCollateral),
      numDigits,
      roundingIntervals
    )
  }

  override def toSubType: ContractDescriptorV1TLV = {
    ContractDescriptorV1TLV(numDigits,
                            outcomeValueFunc.toSubType,
                            roundingIntervals.toTLV,
                            DLCSerializationVersion.current)
  }
}

object NumericContractDescriptor
    extends DLCSpecTypeDeserializable[
      ContractDescriptorV1TLV,
      NumericContractDescriptor](ContractDescriptorV1TLV) {

  override def fromSubType(
      tlv: ContractDescriptorV1TLV): NumericContractDescriptor = {
    NumericContractDescriptor(
      DLCPayoutCurve.fromSubType(tlv.payoutFunction),
      tlv.numDigits,
      RoundingIntervals.fromTLV(tlv.roundingIntervals)
    )
  }
}
