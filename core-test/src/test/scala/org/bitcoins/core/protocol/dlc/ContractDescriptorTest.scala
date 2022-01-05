package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv.{
  ContractDescriptorV1TLV,
  DLCSerializationVersion,
  EnumOutcome
}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class ContractDescriptorTest extends BitcoinSUnitTest {

  behavior of "ContractDescriptor"

  it must "construct a basic enum contract descriptor" in {
    val expectedHex =
      "fda7102903055452554d50000000000000000005424944454e0000000005f5e100035449450000000002faf080"
    val vec: Vector[(EnumOutcome, Satoshis)] = Vector(
      (EnumOutcome("TRUMP"), Satoshis.zero),
      (EnumOutcome("BIDEN"), Bitcoins.one.satoshis),
      (EnumOutcome("TIE"), Satoshis(50000000))
    )
    val contract = EnumContractDescriptor(vec)
    assert(contract.hex == expectedHex)
  }

  it should "fail to create an empty EnumContractDescriptor" in {
    assertThrows[IllegalArgumentException](EnumContractDescriptor(Vector.empty))
  }

  it should "fail for not starting with a endpoint" in {
    assertThrows[IllegalArgumentException] {
      val func = DLCPayoutCurve.polynomialInterpolate(
        Vector(
          PiecewisePolynomialPoint(0, Satoshis(0), isEndpoint = false),
          PiecewisePolynomialPoint(3, Satoshis(100), isEndpoint = true)
        ),
        serializationVersion = DLCSerializationVersion.Beta
      )
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding)
    }
  }

  it should "fail for not ending with a endpoint" in {
    assertThrows[IllegalArgumentException] {
      val func = DLCPayoutCurve.polynomialInterpolate(
        Vector(
          PiecewisePolynomialPoint(0, Satoshis(0), isEndpoint = true),
          PiecewisePolynomialPoint(3, Satoshis(100), isEndpoint = false)
        ),
        serializationVersion = DLCSerializationVersion.Beta
      )
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding)
    }
  }

  it should "fail for starting below the minimum" in {
    val func = DLCPayoutCurve.polynomialInterpolate(
      Vector(
        PiecewisePolynomialPoint(-1, Satoshis(0), isEndpoint = true),
        PiecewisePolynomialPoint(3, Satoshis(100), isEndpoint = true)
      ),
      serializationVersion = DLCSerializationVersion.Beta
    )
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "fail for starting above the minimum" in {
    val func = DLCPayoutCurve.polynomialInterpolate(
      Vector(
        PiecewisePolynomialPoint(1, Satoshis(0), isEndpoint = true),
        PiecewisePolynomialPoint(3, Satoshis(100), isEndpoint = true)
      ),
      serializationVersion = DLCSerializationVersion.Beta
    )
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "fail for ending below the maximum" in {
    val func = DLCPayoutCurve.polynomialInterpolate(
      Vector(
        PiecewisePolynomialPoint(0, Satoshis(0), isEndpoint = true),
        PiecewisePolynomialPoint(2, Satoshis(100), isEndpoint = true)
      ),
      serializationVersion = DLCSerializationVersion.Beta
    )
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "fail for ending above the maximum" in {
    val func = DLCPayoutCurve.polynomialInterpolate(
      Vector(
        PiecewisePolynomialPoint(0, Satoshis(0), isEndpoint = true),
        PiecewisePolynomialPoint(4, Satoshis(100), isEndpoint = true)
      ),
      serializationVersion = DLCSerializationVersion.Beta
    )
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "parse a numeric contract descriptor pre 144" in {
    //we have to be able to parse old numeric contract descriptors
    //pre pr 144 on the DLC spec as we have old wallets deployed with this
    //https://github.com/discreetlogcontracts/dlcspecs/pull/144
    val func = DLCPayoutCurve.polynomialInterpolate(
      Vector(
        PiecewisePolynomialPoint(outcome = 0,
                                 payout = Satoshis(0),
                                 isEndpoint = true),
        PiecewisePolynomialPoint(outcome = 3,
                                 payout = Satoshis(100),
                                 isEndpoint = true)
      ),
      serializationVersion = DLCSerializationVersion.Beta
    )

    val expected =
      NumericContractDescriptor(outcomeValueFunc = func,
                                numDigits = 2,
                                roundingIntervals =
                                  RoundingIntervals.noRounding)

    val oldHex =
      "fda720260002fda7261a0002010000000000000000000000010300000000000000640000fda724020000"

    val actual = ContractDescriptorV1TLV.fromHex(oldHex)

    assert(actual.hex == expected.toTLV.hex)
  }
}
