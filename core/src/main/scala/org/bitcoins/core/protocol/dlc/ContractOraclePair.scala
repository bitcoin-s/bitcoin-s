package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.protocol.tlv.{
  EnumEventDescriptorV0TLV,
  EnumOutcome,
  NumericEventDescriptorTLV
}

/** A pair of [[ContractDescriptor]] and [[OracleInfo]]
  * This type is meant to ensure consistentcy between various
  * [[ContractDescriptor]] and [[OracleInfo]] so that you cannot
  * have an incorrect pairing.
  */
sealed trait ContractOraclePair {
  def contractDescriptor: ContractDescriptor
  def oracleInfo: OracleInfo
}

object ContractOraclePair {

  case class EnumPair(
      contractDescriptor: EnumContractDescriptor,
      oracleInfo: EnumOracleInfo)
      extends ContractOraclePair {

    private val descriptorOutcomes =
      contractDescriptor.map(_._1).sortBy(_.outcome)

    private val isValid = oracleInfo.singleOracleInfos.forall { singleInfo =>
      val announcementOutcomes =
        singleInfo.announcement.eventTLV.eventDescriptor
          .asInstanceOf[EnumEventDescriptorV0TLV]
          .outcomes
          .map(EnumOutcome(_))
          .sortBy(_.outcome)

      announcementOutcomes == descriptorOutcomes
    }

    require(isValid, s"OracleInfo did not match ContractDescriptor: $this")
  }

  case class NumericPair(
      contractDescriptor: NumericContractDescriptor,
      oracleInfo: NumericOracleInfo)
      extends ContractOraclePair {

    private val isValid = oracleInfo.singleOracleInfos.forall { singleInfo =>
      val announcementDescriptor =
        singleInfo.announcement.eventTLV.eventDescriptor
          .asInstanceOf[NumericEventDescriptorTLV]
      announcementDescriptor.base.toInt == 2 && announcementDescriptor.noncesNeeded == contractDescriptor.numDigits
    }

    require(isValid, s"OracleInfo did not match ContractDescriptor: $this")
  }

  /** Returns a valid [[ContractOraclePair]] if the
    * [[ContractDescriptor]] and [[OracleInfo]] are of the same type
    */
  def fromDescriptorOracleOpt(
      descriptor: ContractDescriptor,
      oracleInfo: OracleInfo): Option[ContractOraclePair] = {
    (descriptor, oracleInfo) match {
      case (e: EnumContractDescriptor, o: EnumOracleInfo) =>
        Some(EnumPair(e, o))
      case (n: NumericContractDescriptor, o: NumericOracleInfo) =>
        Some(NumericPair(n, o))
      case (_: EnumContractDescriptor, _: NumericOracleInfo) =>
        None
      case (_: NumericContractDescriptor, _: EnumOracleInfo) =>
        None
    }
  }

  /** Returns a valid [[ContractOraclePair]] if the
    * [[ContractDescriptor]] and [[OracleInfo]] are of the same type
    */
  def fromDescriptorOracle(
      descriptor: ContractDescriptor,
      oracleInfo: OracleInfo): ContractOraclePair = {
    fromDescriptorOracleOpt(descriptor, oracleInfo) match {
      case Some(pair) => pair
      case None =>
        sys.error(
          s"You passed in an incompatible contract/oracle pair, contract=$descriptor, oracle=$oracleInfo")
    }
  }
}
