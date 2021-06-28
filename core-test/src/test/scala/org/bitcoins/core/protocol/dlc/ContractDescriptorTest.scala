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
    val func = DLCPayoutCurve(
      Vector(
        OutcomePayoutPoint(0, Satoshis(0), isEndpoint = false),
        OutcomePayoutPoint(3, Satoshis(100), isEndpoint = true)
      ))
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "fail for not ending with a endpoint" in {
    val func = DLCPayoutCurve(
      Vector(
        OutcomePayoutPoint(0, Satoshis(0), isEndpoint = true),
        OutcomePayoutPoint(3, Satoshis(100), isEndpoint = false)
      ))
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "fail for starting below the minimum" in {
    val func = DLCPayoutCurve(
      Vector(
        OutcomePayoutPoint(-1, Satoshis(0), isEndpoint = true),
        OutcomePayoutPoint(3, Satoshis(100), isEndpoint = true)
      ))
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "fail for starting above the minimum" in {
    val func = DLCPayoutCurve(
      Vector(
        OutcomePayoutPoint(1, Satoshis(0), isEndpoint = true),
        OutcomePayoutPoint(3, Satoshis(100), isEndpoint = true)
      ))
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "fail for starting below the maximum" in {
    val func = DLCPayoutCurve(
      Vector(
        OutcomePayoutPoint(0, Satoshis(0), isEndpoint = true),
        OutcomePayoutPoint(2, Satoshis(100), isEndpoint = true)
      ))
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "fail for starting above the maximum" in {
    val func = DLCPayoutCurve(
      Vector(
        OutcomePayoutPoint(0, Satoshis(0), isEndpoint = true),
        OutcomePayoutPoint(4, Satoshis(100), isEndpoint = true)
      ))
    assertThrows[IllegalArgumentException](
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding))
  }

  it should "correctly create a NumericContractDescriptor" in {
    val func = DLCPayoutCurve(
      Vector(
        OutcomePayoutPoint(0, Satoshis(0), isEndpoint = true),
        OutcomePayoutPoint(3, Satoshis(100), isEndpoint = true)
      ))

    val descriptor =
      NumericContractDescriptor(func, 2, RoundingIntervals.noRounding)

    assert(descriptor.toTLV == ContractDescriptorV1TLV(
      "fda720260002fda7261a0002010000000000000000000000010300000000000000640000fda724020000"))
  }
}
