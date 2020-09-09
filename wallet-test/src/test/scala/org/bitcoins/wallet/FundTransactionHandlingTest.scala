package org.bitcoins.wallet

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.builder.RawTxSigner
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.StorageLocationTag.HotStorage
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.testkit.util.TestUtil
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTest,
  WalletTestUtil,
  WalletWithBitcoind
}
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future

class FundTransactionHandlingTest extends BitcoinSWalletTest {

  override type FixtureParam = WalletWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWalletAndBitcoind(test, getBIP39PasswordOpt())
  }

  val destination: TransactionOutput =
    TransactionOutput(Bitcoins(0.5), TestUtil.p2pkhScriptPubKey)
  val feeRate: SatoshisPerVirtualByte = SatoshisPerVirtualByte.one

  it must "fund a simple raw transaction that requires one utxo" in {
    fundedWallet: WalletWithBitcoind =>
      val wallet = fundedWallet.wallet
      val fundedTxF = wallet.fundRawTransaction(destinations =
                                                  Vector(destination),
                                                feeRate = feeRate,
                                                fromTagOpt = None,
                                                markAsReserved = false)
      for {
        fundedTx <- fundedTxF
      } yield {
        assert(fundedTx.inputs.length == 1,
               s"We should only need one input to fund this tx")
        assert(fundedTx.outputs.contains(destination))
        assert(fundedTx.outputs.length == 2,
               s"We must have a single destination output and a change output")
      }
  }

  it must "fund a transaction that requires all utxos in our wallet" in {
    fundedWallet: WalletWithBitcoind =>
      val amt = Bitcoins(5.5)
      val newDestination = destination.copy(value = amt)
      val wallet = fundedWallet.wallet
      val fundedTxF = wallet.fundRawTransaction(destinations =
                                                  Vector(newDestination),
                                                feeRate = feeRate,
                                                fromTagOpt = None,
                                                markAsReserved = false)
      for {
        fundedTx <- fundedTxF
      } yield {
        assert(fundedTx.inputs.length == 3,
               s"We should need 3 inputs to fund this tx")
        assert(fundedTx.outputs.contains(newDestination))
        assert(fundedTx.outputs.length == 2,
               s"We must have a 2 destination output and a change output")
      }
  }

  it must "not care about the number of destinations" in {
    fundedWallet: WalletWithBitcoind =>
      val destinations = Vector.fill(5)(destination)

      val wallet = fundedWallet.wallet
      val fundedTxF = wallet.fundRawTransaction(destinations = destinations,
                                                feeRate = feeRate,
                                                fromTagOpt = None,
                                                markAsReserved = false)
      for {
        fundedTx <- fundedTxF
      } yield {
        assert(fundedTx.inputs.length == 1,
               s"We should only need one input to fund this tx")

        destinations.foreach(d => assert(fundedTx.outputs.contains(d)))
        assert(fundedTx.outputs.length == 6,
               s"We must have a 6 destination output and a change output")
      }

  }

  it must "fail to fund a raw transaction if we don't have enough money in our wallet" in {
    fundedWallet: WalletWithBitcoind =>
      //our wallet should only have 6 bitcoin in it
      val tooMuchMoney = Bitcoins(10)
      val tooBigOutput = destination.copy(value = tooMuchMoney)
      val wallet = fundedWallet.wallet
      val fundedTxF = wallet.fundRawTransaction(destinations =
                                                  Vector(tooBigOutput),
                                                feeRate = feeRate,
                                                fromTagOpt = None,
                                                markAsReserved = false)

      recoverToSucceededIf[RuntimeException] {
        fundedTxF
      }
  }

  it must "fail to fund a raw transaction if we have the _exact_ amount of money in the wallet because of the fee" in {
    fundedWallet: WalletWithBitcoind =>
      //our wallet should only have 6 bitcoin in it
      val tooMuchMoney = Bitcoins(6)
      val tooBigOutput = destination.copy(value = tooMuchMoney)
      val wallet = fundedWallet.wallet

      //6 bitcoin destination + 1 sat/vbyte fee means we should
      //not have enough money for this
      val fundedTxF = wallet.fundRawTransaction(destinations =
                                                  Vector(tooBigOutput),
                                                feeRate = feeRate,
                                                fromTagOpt = None,
                                                markAsReserved = false)

      recoverToSucceededIf[RuntimeException] {
        fundedTxF
      }
  }

  it must "fund from a specific account" in {
    fundedWallet: WalletWithBitcoind =>
      //we want to fund from account 1, not hte default account
      //account 1 has 1 btc in it
      val amt = Bitcoins(0.1)

      val newDestination = destination.copy(value = amt)
      val wallet = fundedWallet.wallet
      val account1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)
      val account1DbF = wallet.accountDAO.findByAccount(account1)
      for {
        account1DbOpt <- account1DbF
        fundedTx <- wallet.fundRawTransaction(Vector(newDestination),
                                              feeRate,
                                              account1DbOpt.get)
      } yield {
        assert(fundedTx.inputs.nonEmpty)
        assert(fundedTx.outputs.contains(newDestination))
        assert(fundedTx.outputs.length == 2)
      }
  }

  it must "fail to fund from an account that does not have the funds" in {
    fundedWallet: WalletWithBitcoind =>
      //account 1 should only have 1 btc in it
      val amt = Bitcoins(1.1)

      val newDestination = destination.copy(value = amt)
      val wallet = fundedWallet.wallet
      val account1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)
      val account1DbF = wallet.accountDAO.findByAccount(account1)
      val fundedTxF = for {
        account1DbOpt <- account1DbF
        fundedTx <- wallet.fundRawTransaction(Vector(newDestination),
                                              feeRate,
                                              account1DbOpt.get)
      } yield fundedTx

      recoverToSucceededIf[RuntimeException] {
        fundedTxF
      }
  }

  it must "fail to fund from an account with only immature coinbases" in {
    fundedWallet: WalletWithBitcoind =>
      val wallet = fundedWallet.wallet
      val bitcoind = fundedWallet.bitcoind

      val fundedTxF = for {
        _ <- wallet.createNewAccount(wallet.keyManager.kmParams)
        accounts <- wallet.accountDAO.findAll()
        account2 = accounts.find(_.hdAccount.index == 2).get

        addr <- wallet.getNewAddress(account2)

        hash <- bitcoind.generateToAddress(1, addr).map(_.head)
        block <- bitcoind.getBlockRaw(hash)
        _ <- wallet.processBlock(block)

        utxos <- wallet.listUtxos(account2.hdAccount)
        _ = assert(utxos.size == 1)

        fundedTx <-
          wallet.fundRawTransaction(Vector(destination), feeRate, account2)
      } yield fundedTx

      recoverToSucceededIf[RuntimeException] {
        fundedTxF
      }
  }

  it must "mark utxos as reserved after building a transaction" in {
    fundedWallet: WalletWithBitcoind =>
      val wallet = fundedWallet.wallet
      val fundedTxF = wallet.fundRawTransaction(destinations =
                                                  Vector(destination),
                                                feeRate = feeRate,
                                                fromTagOpt = None,
                                                markAsReserved = true)
      for {
        fundedTx <- fundedTxF
        spendingInfos <- wallet.spendingInfoDAO.findOutputsBeingSpent(fundedTx)
        reserved <- wallet.spendingInfoDAO.findByTxoState(TxoState.Reserved)
      } yield {
        assert(spendingInfos.exists(_.state == TxoState.Reserved))
        assert(reserved.size == spendingInfos.size)
      }
  }

  def testAddressTagFunding(
      wallet: Wallet,
      tag: AddressTag): Future[Assertion] = {
    for {
      account <- wallet.getDefaultAccount()

      taggedAddr <- wallet.getNewAddress(Vector(tag))
      _ <-
        wallet.sendToAddress(taggedAddr, destination.value * 2, Some(feeRate))
      taggedBalance <- wallet.getBalance(tag)
      _ = assert(taggedBalance == destination.value * 2)

      expectedUtxos <- wallet.listUtxos(account.hdAccount, tag)
      (txBuilder, utxoInfos) <-
        wallet
          .fundRawTransactionInternal(
            destinations = Vector(destination),
            feeRate = feeRate,
            fromAccount = account,
            fromTagOpt = Some(tag),
            markAsReserved = true
          )
      utx <- txBuilder.buildTx()
      tx <- RawTxSigner.sign(utx, utxoInfos, feeRate)
    } yield {
      assert(tx.inputs.forall(input =>
        expectedUtxos.exists(_.outPoint == input.previousOutput)))
    }
  }

  it must "fund a transaction with only utxos with an unknown address tag" in {
    fundedWallet: WalletWithBitcoind =>
      val wallet = fundedWallet.wallet
      val exampleTag: UnknownAddressTag =
        UnknownAddressTag("Example", "ExampleTagType")

      testAddressTagFunding(wallet, exampleTag)
  }

  it must "fund a transaction with only utxos with an internal address tag" in {
    fundedWallet: WalletWithBitcoind =>
      val wallet = fundedWallet.wallet

      testAddressTagFunding(wallet, HotStorage)
  }
}
