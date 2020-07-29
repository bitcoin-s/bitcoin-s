package org.bitcoins.wallet

import org.bitcoins.commons.jsonmodels.wallet.CoinSelectionAlgo
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.script.constant.{BytesToPushOntoStack, ScriptConstant}
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.wallet.api.CoinSelector
import org.scalatest.{Assertion, FutureOutcome}
import scodec.bits.ByteVector

import scala.concurrent.Future

class WalletSendingTest extends BitcoinSWalletTest {

  override type FixtureParam = FundedWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFundedWallet(test, getBIP39PasswordOpt())

  behavior of "Wallet"

  val testAddress: BitcoinAddress =
    BitcoinAddress("bcrt1qlhctylgvdsvaanv539rg7hyn0sjkdm23y70kgq")

  val amountToSend: Bitcoins = Bitcoins(0.5)

  val feeRateOpt: Some[SatoshisPerByte] = Some(SatoshisPerByte(Satoshis.one))

  it should "correctly send to an address" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    for {
      tx <- wallet.sendToAddress(testAddress, amountToSend, feeRateOpt)
    } yield {
      assert(
        tx.outputs.contains(
          TransactionOutput(amountToSend, testAddress.scriptPubKey)))
    }
  }

  val addresses = Vector(
    BitcoinAddress("bcrt1qlhctylgvdsvaanv539rg7hyn0sjkdm23y70kgq"),
    BitcoinAddress("bcrt1qf4rju7adz5hpuymkfwvg5s94mydc8llk94v74w"),
    BitcoinAddress("bcrt1q9h9wkz6ad49szfl035wh3qdacuslkp6j9pfp4j"),
    BitcoinAddress("bcrt1q9scvqvyf3ssed8zqnfgk5zttnneatg2aszu5q9")
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
      tx <- wallet.sendToAddresses(addresses, amounts, feeRateOpt)
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
        wallet.sendToAddresses(addresses, amounts.tail, feeRateOpt)

      recoverToSucceededIf[IllegalArgumentException] {
        sendToAddressesF
      }
  }

  it should "fail to send to multiple addresses with an incomplete amount of addresses" in {
    fundedWallet =>
      val wallet = fundedWallet.wallet

      val sendToAddressesF =
        wallet.sendToAddresses(addresses.tail, amounts, feeRateOpt)

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
      tx <- wallet.sendToOutputs(expectedOutputs, feeRateOpt)
    } yield {
      assert(expectedOutputs.diff(tx.outputs).isEmpty)
    }
  }

  def testOpReturnCommitment(
      wallet: Wallet,
      hashMessage: Boolean): Future[Assertion] = {
    val message = "ben was here"

    for {
      tx <- wallet.makeOpReturnCommitment(message,
                                          hashMessage = hashMessage,
                                          feeRateOpt)

      outgoingTxDbOpt <- wallet.outgoingTxDAO.read(tx.txIdBE)
    } yield {
      val opReturnOutputOpt = tx.outputs.find(_.value == 0.satoshis)
      assert(opReturnOutputOpt.isDefined, "Missing output with 0 value")
      val opReturnOutput = opReturnOutputOpt.get

      val messageBytes = if (hashMessage) {
        CryptoUtil.sha256(ByteVector(message.getBytes)).bytes
      } else {
        ByteVector(message.getBytes)
      }

      val expectedAsm =
        Seq(OP_RETURN,
            BytesToPushOntoStack(messageBytes.size),
            ScriptConstant(messageBytes))

      assert(opReturnOutput.scriptPubKey.asm == expectedAsm)

      assert(outgoingTxDbOpt.isDefined, "Missing outgoing tx in database")
      val outgoingTxDb = outgoingTxDbOpt.get

      assert(outgoingTxDb.sentAmount == 0.satoshis)
      val changeOutput = tx.outputs.find(_.value > 0.satoshis).get
      assert(
        outgoingTxDb.actualFee + changeOutput.value == outgoingTxDb.inputAmount)
    }
  }

  it should "correctly make a hashed OP_RETURN commitment" in { fundedWallet =>
    testOpReturnCommitment(fundedWallet.wallet, hashMessage = true)
  }

  it should "correctly make an unhashed OP_RETURN commitment" in {
    fundedWallet =>
      testOpReturnCommitment(fundedWallet.wallet, hashMessage = false)
  }

  it should "fail to make an OP_RETURN commitment that is too long" in {
    fundedWallet =>
      val wallet = fundedWallet.wallet

      recoverToSucceededIf[IllegalArgumentException] {
        wallet.makeOpReturnCommitment(
          "This message is much too long and is over 80 bytes, the limit for OP_RETURN. It should cause an error.",
          hashMessage = false,
          feeRateOpt)
      }
  }

  it should "fail to send to a different network address" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    val sendToAddressesF =
      wallet.sendToAddress(BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"),
                           Satoshis(1000),
                           feeRateOpt)

    recoverToSucceededIf[IllegalArgumentException] {
      sendToAddressesF
    }
  }

  it should "fail to send to different network addresses" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    val addrs = Vector(
      BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"),
      BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"),
      BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"),
      BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa")
    )

    val sendToAddressesF =
      wallet.sendToAddresses(addrs, amounts, feeRateOpt)

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
                                     feeRateOpt)
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
      account <- wallet.getDefaultAccount()
      allUtxos <- wallet.listUtxos(account.hdAccount)
      output = TransactionOutput(amountToSend, testAddress.scriptPubKey)
      expectedUtxos = CoinSelector.selectByAlgo(algo,
                                                allUtxos,
                                                Vector(output),
                                                feeRateOpt.get)
      tx <- wallet.sendWithAlgo(testAddress, amountToSend, feeRateOpt, algo)
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
