package org.bitcoins.core.dlc.template

import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.protocol.dlc.models.ContractOraclePair.NumericPair
import org.bitcoins.core.protocol.dlc.models.{
  ContractInfo,
  DLCPayoutCurve,
  NumericContractDescriptor,
  NumericSingleOracleInfo,
  PiecewisePolynomialPoint,
  RoundingIntervals,
  SingleContractInfo
}
import org.bitcoins.core.protocol.tlv.{
  DLCSerializationVersion,
  DigitDecompositionEventDescriptorV0TLV,
  OracleAnnouncementTLV
}

import scala.util.Try

class ContractForDifferenceTemplate extends DLCTemplate {

  // TODO implement me!
  override def createContractInfo(
      oracleAnnouncementTLV: OracleAnnouncementTLV,
      parameters: Map[String, String]): ContractInfo = {
    require(oracleAnnouncementTLV.eventTLV.eventDescriptor
              .isInstanceOf[DigitDecompositionEventDescriptorV0TLV],
            "Oracle announcement must be a numeric one")
    val strikePrice = parseBigDecimal("strike_price", parameters)
    val totalCollateral = Bitcoins(
      parseBigDecimal("total_collateral", parameters)).satoshis

    val func = DLCPayoutCurve.polynomialInterpolate(
      Vector(
        PiecewisePolynomialPoint(0, Satoshis(0), isEndpoint = true),
        PiecewisePolynomialPoint(strikePrice.toLong,
                                 totalCollateral,
                                 isEndpoint = true)
      ),
      serializationVersion = DLCSerializationVersion.Beta
    )

    val contractDescriptor =
      NumericContractDescriptor(func, 3, RoundingIntervals.noRounding)

    val pair = NumericPair(contractDescriptor,
                           NumericSingleOracleInfo(oracleAnnouncementTLV))
    SingleContractInfo(totalCollateral, pair)
  }

  private def parseBigDecimal(
      key: String,
      parameters: Map[String, String]): BigDecimal = {
    parameters.get(key) match {
      case Some(value) =>
        Try(BigDecimal(value)).getOrElse(
          throw new IllegalArgumentException(
            s"The value of `$key` must be a valid decimal number: `$value`"))
      case None =>
        throw new IllegalArgumentException(s"`$key` parameter is required")
    }
  }
}

object ContractForDifferenceTemplateBuilder extends DLCTemplateBuilder {

  override def `type`: String = "cfd"

  override def create(): DLCTemplate = new ContractForDifferenceTemplate
}
