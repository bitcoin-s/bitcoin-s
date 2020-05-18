package org.bitcoins.wallet

import org.bitcoins.commons.jsonmodels.wallet.CoinSelectionAlgo
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.wallet.api.CoinSelector
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future

class WalletSendingTest extends BitcoinSWalletTest {

  override type FixtureParam = FundedWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFundedWallet(test)

  behavior of "Wallet"

  val testAddress: BitcoinAddress =
    BitcoinAddress("bcrt1qlhctylgvdsvaanv539rg7hyn0sjkdm23y70kgq").get

  val amountToSend: Bitcoins = Bitcoins(0.5)

  val feeRate: SatoshisPerByte = SatoshisPerByte(Satoshis.one)

  it should "correctly send to an address" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    for {
      tx <- wallet.sendToAddress(testAddress, amountToSend, feeRate)
    } yield {
      assert(
        tx.outputs.head == TransactionOutput(amountToSend,
                                             testAddress.scriptPubKey))
    }
  }

  val addresses = Vector(
    BitcoinAddress("bcrt1qlhctylgvdsvaanv539rg7hyn0sjkdm23y70kgq").get,
    BitcoinAddress("bcrt1qf4rju7adz5hpuymkfwvg5s94mydc8llk94v74w").get,
    BitcoinAddress("bcrt1q9h9wkz6ad49szfl035wh3qdacuslkp6j9pfp4j").get,
    BitcoinAddress("bcrt1q9scvqvyf3ssed8zqnfgk5zttnneatg2aszu5q9").get
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

  it should "fail to send to a different network address" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    val sendToAddressesF =
      wallet.sendToAddress(
        BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa").get,
        Satoshis(1000),
        feeRate)

    recoverToSucceededIf[IllegalArgumentException] {
      sendToAddressesF
    }
  }

  it should "fail to send to different network addresses" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    val addrs = Vector(
      BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa").get,
      BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa").get,
      BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa").get,
      BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa").get
    )

    val sendToAddressesF =
      wallet.sendToAddresses(addrs, amounts, feeRate, reserveUtxos = false)

    recoverToSucceededIf[IllegalArgumentException] {
      sendToAddressesF
    }
  }

  it should "correctly send from outpoints" in { fundedWallet =>
    val wallet = fundedWallet.wallet
    for {
      allOutPoints <- wallet.spendingInfoDAO.findAllOutpoints()
      // use half of them
      outPoints = allOutPoints.drop(allOutPoints.size / 2)
      tx <- wallet.sendFromOutPoints(outPoints,
                                     testAddress,
                                     amountToSend,
                                     feeRate)
    } yield {
      assert(outPoints.forall(outPoint =>
               tx.inputs.exists(_.previousOutput == outPoint)),
             "Every outpoint was not included included")
      assert(tx.inputs.size == outPoints.size, "An extra input was added")

      val expectedOutput =
        TransactionOutput(amountToSend, testAddress.scriptPubKey)
      assert(tx.outputs.contains(expectedOutput),
             "Did not contain expected output")
    }
  }

  def testSendWithAlgo(
      wallet: Wallet,
      algo: CoinSelectionAlgo): Future[Assertion] = {
    for {
      allUtxos <- wallet.listUtxos()
      output = TransactionOutput(amountToSend, testAddress.scriptPubKey)
      expectedUtxos = CoinSelector.selectByAlgo(algo,
                                                allUtxos,
                                                Vector(output),
                                                feeRate)
      tx <- wallet.sendWithAlgo(testAddress, amountToSend, feeRate, algo)
    } yield {
      val diff =
        expectedUtxos.map(_.outPoint).diff(tx.inputs.map(_.previousOutput))
      assert(diff.isEmpty, s"Incorrect inputs, $diff")

      val expectedOutput =
        TransactionOutput(amountToSend, testAddress.scriptPubKey)
      assert(tx.outputs.contains(expectedOutput),
             "Did not contain expected output")
    }
  }

  it should "correctly send with accumulate largest" in { fundedWallet =>
    testSendWithAlgo(fundedWallet.wallet, CoinSelectionAlgo.AccumulateLargest)
  }

  it should "correctly send with accumulate smallest" in { fundedWallet =>
    testSendWithAlgo(fundedWallet.wallet,
                     CoinSelectionAlgo.AccumulateSmallestViable)
  }

  it should "correctly send with standard accumulate" in { fundedWallet =>
    testSendWithAlgo(fundedWallet.wallet, CoinSelectionAlgo.StandardAccumulate)
  }
}
