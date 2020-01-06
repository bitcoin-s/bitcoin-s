package org.bitcoins.wallet

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.testkit.util.TestUtil
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.FundedWallet
import org.scalatest.FutureOutcome

class FundTransactionHandlingTest extends BitcoinSWalletTest {

  override type FixtureParam = FundedWallet
  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWallet(test)
  }

  val destination = TransactionOutput(Bitcoins(0.5),TestUtil.p2pkhScriptPubKey)
  val feeRate = SatoshisPerVirtualByte.one

  it must "fund a simple raw transaction that requires one utxo" in { fundedWallet : FundedWallet =>

    val wallet = fundedWallet.wallet
    val fundedTxF = wallet.fundRawTransaction(
      destinations = Vector(destination),
      feeRate = feeRate)
    for {
      fundedTx <- fundedTxF
    } yield {
      assert(fundedTx.inputs.length == 1, s"We should only need one input to fund this tx")
      assert(fundedTx.outputs.contains(destination))
      assert(fundedTx.outputs.length == 2, s"We must have a single destination output and a change output")
    }
  }

  it must "fund a transaction that requires all utxos in our wallet" in { fundedWallet: FundedWallet =>
    val amt = Bitcoins(5.5)
    val newDestination = destination.copy(value = amt)
    val wallet = fundedWallet.wallet
    val fundedTxF = wallet.fundRawTransaction(
      destinations = Vector(newDestination),
      feeRate = feeRate)
    for {
      fundedTx <- fundedTxF
    } yield {
      assert(fundedTx.inputs.length == 3, s"We should need 3 inputs to fund this tx")
      assert(fundedTx.outputs.contains(newDestination))
      assert(fundedTx.outputs.length == 2, s"We must have a 2 destination output and a change output")
    }
  }

  it must "not care about the number of destinations" in { fundedWallet: FundedWallet =>
    val destinations = Vector.fill(5)(destination)

    val wallet = fundedWallet.wallet
    val fundedTxF = wallet.fundRawTransaction(
      destinations = destinations,
      feeRate = feeRate)
    for {
      fundedTx <- fundedTxF
    } yield {
      assert(fundedTx.inputs.length == 1, s"We should only need one input to fund this tx")

      destinations.foreach(d =>
        assert(fundedTx.outputs.contains(d))
      )
      assert(fundedTx.outputs.length == 6, s"We must have a 6 destination output and a change output")
    }

  }

  it must "fail to fund a raw transaction if we don't have enough money in our wallet" in { fundedWallet: FundedWallet =>
    //our wallet should only have 6 bitcoin in it
    val tooMuchMoney = Bitcoins(10)
    val tooBigOutput = destination.copy(value = tooMuchMoney)
    val wallet = fundedWallet.wallet
    val fundedTxF = wallet.fundRawTransaction(
      destinations = Vector(tooBigOutput),
      feeRate = feeRate)

    recoverToSucceededIf[RuntimeException] {
      fundedTxF
    }
  }

  it must "fail to fund a raw transaction if we have the _exact_ amount of money in the wallet because of the fee" in {fundedWallet: FundedWallet =>
    //our wallet should only have 6 bitcoin in it
    val tooMuchMoney = Bitcoins(6)
    val tooBigOutput = destination.copy(value = tooMuchMoney)
    val wallet = fundedWallet.wallet

    //6 bitcoin destination + 1 sat/vbyte fee means we should
    //not have enough money for this
    val fundedTxF = wallet.fundRawTransaction(
      destinations = Vector(tooBigOutput),
      feeRate = feeRate)

    recoverToSucceededIf[RuntimeException] {
      fundedTxF
    }
  }

  it must "fund from a specific account" ignore { _: FundedWallet =>
    assert(false)

  }
}
