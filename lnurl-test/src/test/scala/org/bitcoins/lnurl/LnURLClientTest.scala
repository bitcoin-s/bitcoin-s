package org.bitcoins.lnurl

import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.lnurl.json.LnURLJsonModels._
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class LnURLClientTest extends BitcoinSAsyncTest {
  val client = new LnURLClient(None)

  it must "make a pay request" in {
    val lnurl = LnURL.fromString(
      "LNURL1DP68GURN8GHJ7MRWW4EXCTNXD9SHG6NPVCHXXMMD9AKXUATJDSKHQCTE8AEK2UMND9HKU0TPXUCRJVEHXV6RYVEEX5CNSCESXYURSEP5VE3RWCNP89JRJD33XUEXGCFNX5URZCT9XYMRQDRXVVCKZVRPV43KYV3E8YMKZE3H89SNWEVZTNK")

    client.makeRequest(lnurl).map {
      case pay: LnURLPayResponse =>
        assert(pay.maxSendable >= MilliSatoshis.zero)
        assert(pay.minSendable >= MilliSatoshis.zero)
      case _: LnURLWithdrawResponse =>
        fail("Incorrect response parsed")
    }
  }

  it must "make a pay request and get the invoice" in {
    val lnurl = LnURL.fromString(
      "LNURL1DP68GURN8GHJ7MRWW4EXCTNXD9SHG6NPVCHXXMMD9AKXUATJDSKHQCTE8AEK2UMND9HKU0TPXUCRJVEHXV6RYVEEX5CNSCESXYURSEP5VE3RWCNP89JRJD33XUEXGCFNX5URZCT9XYMRQDRXVVCKZVRPV43KYV3E8YMKZE3H89SNWEVZTNK")

    client.makeRequest(lnurl).flatMap {
      case pay: LnURLPayResponse =>
        val amt = pay.minSendable.toLnCurrencyUnit
        client.getInvoice(pay, amt).map { inv =>
          assert(inv.amount.contains(amt))
        }
      case _: LnURLWithdrawResponse =>
        fail("Incorrect response parsed")
    }
  }

  it must "make a withdrawal request" in {
    val lnurl = LnURL.fromString(
      "LNURL1DP68GURN8GHJ7MRWW4EXCTNXD9SHG6NPVCHXXMMD9AKXUATJDSKHW6T5DPJ8YCTH8AEK2UMND9HKU0FJVSCNZDPHVYENVDTPVYCRSVMPXVMRSCEEXGERQVPSXV6X2C3KX9JXZVMZ8PNXZDR9VY6N2DRZVG6RWEPCVYMRZDMRV9SK2D3KV43XVCF58DT")

    client.makeRequest(lnurl).map {
      case _: LnURLPayResponse =>
        fail("Incorrect response parsed")
      case w: LnURLWithdrawResponse =>
        assert(w.defaultDescription.nonEmpty)
        assert(w.k1.nonEmpty)
        assert(w.maxWithdrawable >= MilliSatoshis.zero)
        assert(w.minWithdrawable >= MilliSatoshis.zero)
    }
  }

  it must "make a withdrawal request and do withdrawal" in {
    val lnurl = LnURL.fromString(
      "LNURL1DP68GURN8GHJ7MRWW4EXCTNXD9SHG6NPVCHXXMMD9AKXUATJDSKHW6T5DPJ8YCTH8AEK2UMND9HKU0FJVSCNZDPHVYENVDTPVYCRSVMPXVMRSCEEXGERQVPSXV6X2C3KX9JXZVMZ8PNXZDR9VY6N2DRZVG6RWEPCVYMRZDMRV9SK2D3KV43XVCF58DT")

    val inv = LnInvoice.fromString(
      "lnbc1302470n1p3x3ssapp5axqf6dsusf98895vdhw97rn0szk4z6cxa5hfw3s2q5ksn3575qssdzz2pskjepqw3hjqnmsv4h9xct5wvszsnmjv3jhygzfgsazqem9dejhyctvtan82mny9ycqzpgxqzuysp5q97feeev2tnjsc0qn9kezqlgs8eekwfkxsc28uwxp9elnzkj2n0s9qyyssq02hkrz7dr0adx09t6w2tr9k8nczvq094r7qx297tsdupgeg5t3m8hvmkl7mqhtvx94he3swlg2qzhqk2j39wehcmv9awc06gex82e8qq0u0pm6")

    client.makeRequest(lnurl).flatMap {
      case _: LnURLPayResponse =>
        fail("Incorrect response parsed")
      case w: LnURLWithdrawResponse =>
        client.doWithdrawal(w, inv).map(bool => assert(bool))
    }
  }
}
