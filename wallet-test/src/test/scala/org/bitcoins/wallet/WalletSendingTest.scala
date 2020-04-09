package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.scalatest.FutureOutcome

class WalletSendingTest extends BitcoinSWalletTest {

  override type FixtureParam = FundedWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFundedWallet(test)

  behavior of "Wallet"

  val feeRate: SatoshisPerByte = SatoshisPerByte(Satoshis.one)

  it should "correctly send to an address" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    val testAddress = BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa").get
    val amountToSend: Bitcoins = Bitcoins(0.5)

    for {
      tx <- wallet.sendToAddress(testAddress, amountToSend, feeRate)
    } yield {
      assert(
        tx.outputs.head == TransactionOutput(amountToSend,
                                             testAddress.scriptPubKey))
    }
  }

  val addresses = Vector(
    BitcoinAddress("tb1q6cmkvmyachk0jhljv7r0ey04a580cj3wnkvhff").get,
    BitcoinAddress("n43ybT6wR9NZP2im4VPBZK14hmr7Kt79mx").get,
    BitcoinAddress("2N16oE62ZjAPup985dFBQYAuy5zpDraH7Hk").get,
    BitcoinAddress("2N3jP8HLLA3ihGGi3T7AaLUapVXu8LkGDqm").get
  )

  val amounts = Vector(
    Bitcoins(0.5),
    Bitcoins(0.25),
    Bitcoins(1),
    Bitcoins(3)
  )

  it should "correctly send to multiple addresses" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    for {
      tx <- wallet.sendToAddresses(addresses,
                                   amounts,
                                   feeRate,
                                   reserveUtxos = false)
    } yield {
      val expectedOutputs = addresses.zip(amounts).map {
        case (addr, amount) => TransactionOutput(amount, addr.scriptPubKey)
      }
      assert(expectedOutputs.diff(tx.outputs).isEmpty)
    }
  }

  it should "fail to send to multiple addresses with an incomplete amount of amounts" in {
    fundedWallet =>
      val wallet = fundedWallet.wallet

      val sendToAddressesF =
        wallet.sendToAddresses(addresses,
                               amounts.tail,
                               feeRate,
                               reserveUtxos = false)

      recoverToSucceededIf[IllegalArgumentException] {
        sendToAddressesF
      }
  }

  it should "fail to send to multiple addresses with an incomplete amount of addresses" in {
    fundedWallet =>
      val wallet = fundedWallet.wallet

      val sendToAddressesF =
        wallet.sendToAddresses(addresses.tail,
                               amounts,
                               feeRate,
                               reserveUtxos = false)

      recoverToSucceededIf[IllegalArgumentException] {
        sendToAddressesF
      }
  }

  it should "correctly send to multiple outputs" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    val expectedOutputs = addresses.zip(amounts).map {
      case (addr, amount) => TransactionOutput(amount, addr.scriptPubKey)
    }

    for {
      tx <- wallet.sendToOutputs(expectedOutputs, feeRate, reserveUtxos = false)
    } yield {
      assert(expectedOutputs.diff(tx.outputs).isEmpty)
    }
  }
}
