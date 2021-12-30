package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv.ContractDescriptorV1TLV
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class ContractDescriptorTest extends BitcoinSUnitTest {

  behavior of "ContractDescriptor"

  it should "fail to create an empty EnumContractDescriptor" in {
    assertThrows[IllegalArgumentException](EnumContractDescriptor(Vector.empty))
  }

  it should "fail for not starting with a endpoint" in {
    assertThrows[IllegalArgumentException] {
      val func = DLCPayoutCurve.polynomialInterpolate(
        Vector(
          PiecewisePolynomialPoint(0, Satoshis(0), isEndpoint = false),
          PiecewisePolynomialPoint(3, Satoshis(100), isEndpoint = true)
        ))
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding)
    }
  }

  it should "fail for not ending with a endpoint" in {
    assertThrows[IllegalArgumentException] {
      val func = DLCPayoutCurve.polynomialInterpolate(
        Vector(
          PiecewisePolynomialPoint(0, Satoshis(0), isEndpoint = true),
          PiecewisePolynomialPoint(3, Satoshis(100), isEndpoint = false)
        ))
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding)
    }
  }

  it should "fail for starting below the minimum" in {
    val func = DLCPayoutCurve.polynomialInterpolate(
      Vector(
        PiecewisePolynomialPoint(-1, Satoshis(0), isEndpoint = true),
        PiecewisePolynomialPoint(3, Satoshis(100), isEndpoint = true)
      ))
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "fail for starting above the minimum" in {
    val func = DLCPayoutCurve.polynomialInterpolate(
      Vector(
        PiecewisePolynomialPoint(1, Satoshis(0), isEndpoint = true),
        PiecewisePolynomialPoint(3, Satoshis(100), isEndpoint = true)
      ))
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "fail for ending below the maximum" in {
    val func = DLCPayoutCurve.polynomialInterpolate(
      Vector(
        PiecewisePolynomialPoint(0, Satoshis(0), isEndpoint = true),
        PiecewisePolynomialPoint(2, Satoshis(100), isEndpoint = true)
      ))
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "fail for ending above the maximum" in {
    val func = DLCPayoutCurve.polynomialInterpolate(
      Vector(
        PiecewisePolynomialPoint(0, Satoshis(0), isEndpoint = true),
        PiecewisePolynomialPoint(4, Satoshis(100), isEndpoint = true)
      ))
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "correctly create a NumericContractDescriptor" in {
    val func = DLCPayoutCurve.polynomialInterpolate(
      Vector(
        PiecewisePolynomialPoint(0, Satoshis(0), isEndpoint = true),
        PiecewisePolynomialPoint(3, Satoshis(100), isEndpoint = true)
      ))

    val descriptor =
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding)

    //i need to write a test case to make sure we can parse both the old serialization
    //format and the new serialization format
    //i also need to verify that the data structures above are correct in the diff
    val expected = ContractDescriptorV1TLV.fromHex(
      "fda720260002fda7261a0002010000000000000000000000010300000000000000640000fda724020000")
    assert(descriptor.toTLV == expected)
  }
}
