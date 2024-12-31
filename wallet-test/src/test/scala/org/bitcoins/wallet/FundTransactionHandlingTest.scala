package org.bitcoins.wallet

import org.bitcoins.core.api.wallet.NeutrinoHDWalletApi
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.utxo.StorageLocationTag.HotStorage
import org.bitcoins.core.wallet.utxo.*
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTestCachedBitcoindNewest,
  WalletTestUtil,
  WalletWithBitcoindRpc
}
import org.bitcoins.testkitcore.util.TestUtil
import org.scalatest.{Assertion, FutureOutcome, Outcome}

import scala.concurrent.Future

class FundTransactionHandlingTest
    extends BitcoinSWalletTestCachedBitcoindNewest {

  override type FixtureParam = WalletWithBitcoindRpc

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withFundedWalletAndBitcoindCached(test, bitcoind)(
        getFreshWalletAppConfig
      )
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  val destination: TransactionOutput =
    TransactionOutput(Bitcoins(0.5), TestUtil.p2pkhScriptPubKey)

  it must "fund a simple raw transaction that requires one utxo" in {
    (fundedWallet: WalletWithBitcoindRpc) =>
      val wallet = fundedWallet.wallet
      for {
        feeRate <- wallet.getFeeRate()
        fundRawTxHelper <- wallet.fundTxHandling.fundRawTransaction(
          destinations = Vector(destination),
          feeRate = feeRate,
          fromTagOpt = None,
          markAsReserved = false
        )
      } yield {
        val fundedTx = fundRawTxHelper.unsignedTx
        assert(
          fundedTx.inputs.length == 1,
          s"We should only need one input to fund this tx"
        )
        assert(fundedTx.outputs.contains(destination))
        assert(
          fundedTx.outputs.length == 2,
          s"We must have a single destination output and a change output"
        )
      }
  }

  it must "fund a transaction that requires all utxos in our wallet" in {
    (fundedWallet: WalletWithBitcoindRpc) =>
      val amt = Bitcoins(5.5)
      val newDestination = destination.copy(value = amt)
      val wallet = fundedWallet.wallet
      for {
        feeRate <- wallet.getFeeRate()
        fundRawTxHelper <- wallet.fundTxHandling.fundRawTransaction(
          destinations = Vector(newDestination),
          feeRate = feeRate,
          fromTagOpt = None,
          markAsReserved = false
        )
      } yield {
        val fundedTx = fundRawTxHelper.unsignedTx
        assert(
          fundedTx.inputs.length == 3,
          s"We should need 3 inputs to fund this tx"
        )
        assert(fundedTx.outputs.contains(newDestination))
        assert(
          fundedTx.outputs.length == 2,
          s"We must have a 2 destination output and a change output"
        )
      }
  }

  it must "not care about the number of destinations" in {
    (fundedWallet: WalletWithBitcoindRpc) =>
      val destinations = Vector.fill(5)(destination)
      val wallet = fundedWallet.wallet

      for {
        feeRate <- wallet.getFeeRate()
        fundRawTxHelper <- wallet.fundTxHandling.fundRawTransaction(
          destinations = destinations,
          feeRate = feeRate,
          fromTagOpt = None,
          markAsReserved = false
        )
      } yield {
        val fundedTx = fundRawTxHelper.unsignedTx
        // Can be different depending on waste calculation
        assert(
          fundedTx.inputs.length == 1 || fundedTx.inputs.length == 2,
          s"We should only need one or two inputs to fund this tx"
        )

        destinations.foreach(d => assert(fundedTx.outputs.contains(d)))
        assert(
          fundedTx.outputs.length == 6,
          s"We must have a 6 destination output and a change output"
        )
      }

  }

  it must "fail to fund a raw transaction if we don't have enough money in our wallet" in {
    (fundedWallet: WalletWithBitcoindRpc) =>
      // our wallet should only have 6 bitcoin in it
      val tooMuchMoney = Bitcoins(10)
      val tooBigOutput = destination.copy(value = tooMuchMoney)
      val wallet = fundedWallet.wallet

      val fundedTxF = for {
        feeRate <- wallet.getFeeRate()
        fundedTx <- wallet.fundTxHandling.fundRawTransaction(
          destinations = Vector(tooBigOutput),
          feeRate = feeRate,
          fromTagOpt = None,
          markAsReserved = false
        )
      } yield fundedTx

      recoverToSucceededIf[RuntimeException] {
        fundedTxF
      }
  }

  it must "fail to fund a raw transaction if we have the _exact_ amount of money in the wallet because of the fee" in {
    (fundedWallet: WalletWithBitcoindRpc) =>
      // our wallet should only have 6 bitcoin in it
      val tooMuchMoney = Bitcoins(6)
      val tooBigOutput = destination.copy(value = tooMuchMoney)
      val wallet = fundedWallet.wallet

      val fundedTxF = for {
        feeRate <- wallet.getFeeRate()
        fundedTx <- wallet.fundTxHandling.fundRawTransaction(
          destinations = Vector(tooBigOutput),
          feeRate = feeRate,
          fromTagOpt = None,
          markAsReserved = false
        )
      } yield fundedTx

      recoverToSucceededIf[RuntimeException] {
        fundedTxF
      }
  }

  it must "fund from a specific account" in {
    (fundedWallet: WalletWithBitcoindRpc) =>
      // we want to fund from account 1, not hte default account
      // account 1 has 1 btc in it
      val amt = Bitcoins(0.1)
      val newDestination = destination.copy(value = amt)
      val wallet = fundedWallet.wallet
      val walletConfig = fundedWallet.walletConfig
      val account1 = WalletTestUtil.getHdAccount1(walletConfig)
      val account1DbF = wallet.accountHandling.findAccount(account1)
      for {
        feeRate <- wallet.getFeeRate()
        account1DbOpt <- account1DbF
        fundRawTxHelper <- wallet.fundTxHandling.fundRawTransaction(
          Vector(newDestination),
          feeRate,
          account1DbOpt.get,
          markAsReserved = true
        )
      } yield {
        val fundedTx = fundRawTxHelper.unsignedTx
        assert(fundedTx.inputs.nonEmpty)
        assert(fundedTx.outputs.contains(newDestination))
        assert(fundedTx.outputs.length == 2)
      }
  }

  it must "fail to fund from an account that does not have the funds" in {
    (fundedWallet: WalletWithBitcoindRpc) =>
      // account 1 should only have 1 btc in it
      val amt = Bitcoins(1.1)
      val newDestination = destination.copy(value = amt)
      val wallet = fundedWallet.wallet
      val walletConfig = fundedWallet.walletConfig
      val account1 = WalletTestUtil.getHdAccount1(walletConfig)
      val account1DbF = wallet.accountHandling.findAccount(account1)
      val fundedTxF = for {
        feeRate <- wallet.getFeeRate()
        account1DbOpt <- account1DbF
        fundedTx <- wallet.fundTxHandling.fundRawTransaction(
          destinations = Vector(newDestination),
          feeRate = feeRate,
          fromAccount = account1DbOpt.get,
          markAsReserved = true
        )
      } yield fundedTx

      recoverToSucceededIf[RuntimeException] {
        fundedTxF
      }
  }

  it must "fail to fund from an account with only immature coinbases" in {
    (fundedWallet: WalletWithBitcoindRpc) =>
      val wallet = fundedWallet.wallet
      val bitcoind = fundedWallet.bitcoind
      val fundedTxF = for {
        feeRate <- wallet.getFeeRate()
        accounts <- wallet.accountHandling.getAccounts()
        account2 = accounts.find(_.hdAccount.index == 2).get

        addr <- wallet.accountHandling.getNewAddress(account2)

        hash <- bitcoind.generateToAddress(1, addr).map(_.head)
        block <- bitcoind.getBlockRaw(hash)
        _ <- wallet.transactionProcessing.processBlock(block)

        utxos <- wallet.utxoHandling.getUtxos(account2.hdAccount)
        _ = assert(utxos.size == 1)

        fundedTx <-
          wallet.fundTxHandling.fundRawTransaction(
            destinations = Vector(destination),
            feeRate = feeRate,
            fromAccount = account2,
            markAsReserved = true
          )
      } yield fundedTx

      recoverToSucceededIf[RuntimeException] {
        fundedTxF
      }
  }

  it must "mark utxos as reserved after building a transaction" in {
    (fundedWallet: WalletWithBitcoindRpc) =>
      val wallet = fundedWallet.wallet
      for {
        feeRate <- wallet.getFeeRate()
        fundRawTxHelper <- wallet.fundTxHandling.fundRawTransaction(
          destinations = Vector(destination),
          feeRate = feeRate,
          fromTagOpt = None,
          markAsReserved = true
        )

        spendingInfos <- wallet.utxoHandling.findOutputsBeingSpent(
          fundRawTxHelper.unsignedTx
        )
        reserved <- wallet.utxoHandling.getUtxos(TxoState.Reserved)
      } yield {
        assert(spendingInfos.exists(_.state == TxoState.Reserved))
        assert(reserved.size == spendingInfos.size)
      }
  }

  def testAddressTagFunding(
      wallet: NeutrinoHDWalletApi,
      tag: AddressTag
  ): Future[Assertion] = {
    for {
      feeRate <- wallet.getFeeRate()
      taggedAddr <- wallet.addressHandling.getNewAddress(Vector(tag))
      _ <-
        wallet.sendFundsHandling.sendToAddress(taggedAddr,
                                               destination.value * 2,
                                               Some(feeRate))
      taggedBalance <- wallet.utxoHandling.getBalance(tag)
      _ = assert(taggedBalance == destination.value * 2)

      expectedUtxos <- wallet.utxoHandling.getUtxos(tag)
      fundRawTxHelper <-
        wallet.fundTxHandling
          .fundRawTransaction(
            destinations = Vector(destination),
            feeRate = feeRate,
            fromTagOpt = Some(tag),
            markAsReserved = true
          )
    } yield {
      val tx = fundRawTxHelper.signedTx

      assert(
        tx.inputs.forall(input =>
          expectedUtxos.exists(_.outPoint == input.previousOutput))
      )
    }
  }

  it must "fund a transaction with only utxos with an unknown address tag" in {
    (fundedWallet: WalletWithBitcoindRpc) =>
      val wallet = fundedWallet.wallet
      val exampleTag: UnknownAddressTag =
        UnknownAddressTag("Example", "ExampleTagType")

      testAddressTagFunding(wallet, exampleTag)
  }

  it must "fund a transaction with only utxos with an internal address tag" in {
    (fundedWallet: WalletWithBitcoindRpc) =>
      val wallet = fundedWallet.wallet

      testAddressTagFunding(wallet, HotStorage)
  }
}
