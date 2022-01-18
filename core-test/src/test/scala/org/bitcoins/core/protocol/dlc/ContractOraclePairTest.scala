package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv.{
  DLCSerializationVersion,
  EnumOutcome,
  OracleAnnouncementV0TLV
}
import org.bitcoins.core.util.sorted.OrderedAnnouncements
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class ContractOraclePairTest extends BitcoinSUnitTest {

  behavior of "ContractOraclePair"

  it should "not be able to construct an invalid enum contract oracle pair" in {
    val contractDescriptor = EnumContractDescriptor(
      Vector("Never", "gonna", "give", "you", "up").map(
        EnumOutcome(_) -> Satoshis.zero))

    def enumOracleInfo(outcomes: Vector[String]): EnumSingleOracleInfo = {
      EnumSingleOracleInfo(
        OracleAnnouncementV0TLV.dummyForEventsAndKeys(
          ECPrivateKey.freshPrivateKey,
          ECPrivateKey.freshPrivateKey.schnorrNonce,
          outcomes.map(EnumOutcome.apply)))
    }

    val oracleInfo1 =
      enumOracleInfo(Vector("Never", "gonna", "let", "you", "down"))
    val oracleInfo2 = enumOracleInfo(
      Vector("Never", "gonna", "run", "around", "and", "desert", "you"))
    val oracleInfo3 =
      enumOracleInfo(Vector("Never", "gonna", "make", "you", "cry"))

    val multiOracleInfo = EnumMultiOracleInfo(
      2,
      OrderedAnnouncements(
        Vector(oracleInfo1, oracleInfo2, oracleInfo3).map(_.announcement)))

    assertThrows[IllegalArgumentException](
      ContractOraclePair.EnumPair(contractDescriptor, oracleInfo1)
    )
    assertThrows[IllegalArgumentException](
      ContractOraclePair.EnumPair(contractDescriptor, oracleInfo2)
    )
    assertThrows[IllegalArgumentException](
      ContractOraclePair.EnumPair(contractDescriptor, oracleInfo3)
    )

    assertThrows[IllegalArgumentException](
      ContractOraclePair.EnumPair(contractDescriptor, multiOracleInfo)
    )
  }

  it should "not be able to construct an invalid numeric contract oracle pair" in {
    val contractDescriptor =
      NumericContractDescriptor(
        DLCPayoutCurve.polynomialInterpolate(
          Vector(PiecewisePolynomialEndpoint(0, 0),
                 PiecewisePolynomialEndpoint((1L << 7) - 1, 1)),
          serializationVersion = DLCSerializationVersion.Beta),
        numDigits = 7,
        RoundingIntervals.noRounding
      )

    def numericOracleInfo(numDigits: Int): NumericSingleOracleInfo = {
      NumericSingleOracleInfo(
        OracleAnnouncementV0TLV.dummyForKeys(
          ECPrivateKey.freshPrivateKey,
          Vector.fill(numDigits)(ECPrivateKey.freshPrivateKey.schnorrNonce)))
    }

    val oracleInfo1 = numericOracleInfo(1)
    val oracleInfo2 = numericOracleInfo(2)
    val oracleInfo3 = numericOracleInfo(3)

    val announcements =
      OrderedAnnouncements(
        Vector(oracleInfo1, oracleInfo2, oracleInfo3).map(_.announcement))
    val multiOracleInfoExact = NumericExactMultiOracleInfo(2, announcements)
    val multiOracleInfo = NumericMultiOracleInfo(2,
                                                 announcements,
                                                 maxErrorExp = 2,
                                                 minFailExp = 1,
                                                 maximizeCoverage = true)

    assertThrows[IllegalArgumentException](
      ContractOraclePair.NumericPair(contractDescriptor, oracleInfo1)
    )
    assertThrows[IllegalArgumentException](
      ContractOraclePair.NumericPair(contractDescriptor, oracleInfo2)
    )
    assertThrows[IllegalArgumentException](
      ContractOraclePair.NumericPair(contractDescriptor, oracleInfo3)
    )

    assertThrows[IllegalArgumentException](
      ContractOraclePair.NumericPair(contractDescriptor, multiOracleInfoExact)
    )
    assertThrows[IllegalArgumentException](
      ContractOraclePair.NumericPair(contractDescriptor, multiOracleInfo)
    )
  }
}
