package org.bitcoins.wallet

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits, Satoshis}
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.rescan.RescanState
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTestCachedBitcoindNewest,
  WalletWithBitcoindRpc
}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class RescanHandlingTest extends BitcoinSWalletTestCachedBitcoindNewest {

  override type FixtureParam = WalletWithBitcoindRpc

  behavior of "Wallet rescans"

  it must "properly clear utxos but not addresses for an account" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet

      for {
        accountDb <- wallet.getDefaultAccount()
        account = accountDb.hdAccount
        utxos <- wallet.listUtxos(account)
        _ = assert(utxos.nonEmpty)

        addresses <- wallet.listAddresses(account)
        _ = assert(addresses.nonEmpty)

        _ <- wallet.clearUtxos(account)

        clearedUtxos <- wallet.listUtxos(account)
        clearedAddresses <- wallet.listAddresses(account)
      } yield {
        assert(clearedUtxos.isEmpty)
        assert(clearedAddresses.nonEmpty)
      }
  }

  it must "properly clear all utxos and address" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet

      for {
        balance <- wallet.getBalance()
        _ = assert(balance != Satoshis.zero)
        utxos <- wallet.listUtxos()
        _ = assert(utxos.nonEmpty)

        addresses <- wallet.listAddresses()
        _ = assert(addresses.nonEmpty)

        _ <- wallet.clearAllUtxos()

        clearedUtxos <- wallet.listUtxos()
        clearedAddresses <- wallet.listAddresses()
      } yield {
        assert(clearedUtxos.isEmpty)
        assert(clearedAddresses.nonEmpty)
      }
  }

  val DEFAULT_ADDR_BATCH_SIZE = 10
  it must "be able to discover funds that belong to the wallet using WalletApi.rescanNeutrinoWallet" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet

      val initBalanceF = wallet.getBalance()

      val rescanF = for {
        initBalance <- initBalanceF
        _ =
          assert(initBalance > CurrencyUnits.zero,
                 s"Cannot run rescan test if our init wallet balance is zero!")
        rescanState <- wallet.fullRescanNeutrinoWallet(DEFAULT_ADDR_BATCH_SIZE)
        _ = assert(rescanState.isInstanceOf[RescanState.RescanStarted])
        _ <- rescanState.asInstanceOf[RescanState.RescanStarted].blocksMatchedF
        balanceAfterRescan <- wallet.getBalance()
      } yield {
        assert(balanceAfterRescan == initBalance)
      }

      rescanF
  }

  it must "be able to discover funds that occurred within a certain range" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet
      val bitcoind = fixture.bitcoind

      val amt = Bitcoins.one
      val numBlocks = 1

      //send funds to a fresh wallet address
      val addrF = wallet.getNewAddress()
      val bitcoindAddrF = bitcoind.getNewAddress
      val initBlockHeightF = wallet.chainQueryApi.getBestHashBlockHeight()
      val newTxWalletF = for {
        addr <- addrF
        txid <- bitcoind.sendToAddress(addr, amt)
        tx <- bitcoind.getRawTransactionRaw(txid)
        bitcoindAddr <- bitcoindAddrF
        blockHashes <-
          bitcoind.generateToAddress(blocks = numBlocks, address = bitcoindAddr)
        newTxWallet <- wallet.processTransaction(transaction = tx,
                                                 blockHashOpt =
                                                   blockHashes.headOption)
        balance <- newTxWallet.getBalance()
        unconfirmedBalance <- newTxWallet.getUnconfirmedBalance()
      } yield {
        //balance doesn't have to exactly equal, as there was money in the
        //wallet before hand.
        assert(balance >= amt)
        assert(amt == unconfirmedBalance)
        newTxWallet
      }

      //let's clear the wallet and then do a rescan for the last numBlocks
      //that means the wallet should only contain the amt we just processed
      for {
        _ <- newTxWalletF
        initBlockHeight <- initBlockHeightF
        txInBlockHeight = initBlockHeight + numBlocks
        txInBlockHeightOpt = Some(BlockStamp.BlockHeight(txInBlockHeight))
        _ <- wallet.clearAllUtxos()
        zeroBalance <- wallet.getBalance()
        _ = assert(zeroBalance == Satoshis.zero)
        rescanState <- wallet.rescanNeutrinoWallet(startOpt =
                                                     txInBlockHeightOpt,
                                                   endOpt = None,
                                                   addressBatchSize =
                                                     DEFAULT_ADDR_BATCH_SIZE,
                                                   useCreationTime = false,
                                                   force = false)
        _ <- {
          rescanState match {
            case started: RescanState.RescanStarted => started.blocksMatchedF
            case _: RescanState                     => Future.unit
          }
        }
        balance <- wallet.getBalance()
        unconfirmedBalance <- wallet.getUnconfirmedBalance()
      } yield {
        assert(balance == amt)
        assert(unconfirmedBalance == Bitcoins(1))
      }
  }

  it must "be able to discover funds using multiple batches" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet
      val bitcoind = fixture.bitcoind

      val amt = Bitcoins.one
      val numBlocks = 1
      val initBalanceF = wallet.getBalance()

      val defaultAccountF = wallet.getDefaultAccount()
      //send funds to a fresh wallet address
      val addrF = wallet.getNewAddress()
      val bitcoindAddrF = bitcoind.getNewAddress
      val balanceAfterPayment1F = for {
        addr <- addrF
        _ <- initBalanceF
        txid <- bitcoind.sendToAddress(addr, amt)
        tx <- bitcoind.getRawTransactionRaw(txid)
        bitcoindAddr <- bitcoindAddrF
        blockHashes <-
          bitcoind.generateToAddress(blocks = numBlocks, address = bitcoindAddr)
        newTxWallet <- wallet.processTransaction(transaction = tx,
                                                 blockHashOpt =
                                                   blockHashes.headOption)
        balance <- newTxWallet.getBalance()
        unconfirmedBalance <- newTxWallet.getUnconfirmedBalance()
      } yield {
        //balance doesn't have to exactly equal, as there was money in the
        //wallet before hand.
        assert(balance >= amt)
        assert(amt == unconfirmedBalance)
        balance
      }

      for {
        _ <- initBalanceF
        balanceAfterPayment1 <- balanceAfterPayment1F

        account <- defaultAccountF
        txIds <-
          wallet
            .listUtxos(account.hdAccount)
            .map(_.map(_.txid))
        _ <- wallet
          .findByTxIds(txIds)
          .map(_.flatMap(_.blockHashOpt))

        _ <- wallet.clearAllUtxos()
        _ <- wallet.clearAllAddresses()
        balanceAfterClear <- wallet.getBalance()
        rescanState <- wallet.fullRescanNeutrinoWallet(1, true)
        _ <- RescanState.awaitRescanDone(rescanState)
        _ <- AsyncUtil.awaitConditionF(
          () => wallet.getBalance().map(_ == balanceAfterPayment1),
          maxTries = 100)
        balanceAfterRescan <- wallet.getBalance()
      } yield {
        assert(balanceAfterClear == CurrencyUnits.zero)
        assert(balanceAfterRescan == balanceAfterPayment1)
      }
  }

  it must "be able to discover funds that occurred from the wallet creation time" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet
      val bitcoind = fixture.bitcoind

      val amt = Bitcoins.one
      val numBlocks = 1

      //send funds to a fresh wallet address
      val addrF = wallet.getNewAddress()
      val bitcoindAddrF = bitcoind.getNewAddress
      val newTxWalletF = for {
        addr <- addrF
        txid <- bitcoind.sendToAddress(addr, amt)
        tx <- bitcoind.getRawTransactionRaw(txid)
        bitcoindAddr <- bitcoindAddrF
        blockHashes <-
          bitcoind.generateToAddress(blocks = numBlocks, address = bitcoindAddr)
        newTxWallet <- wallet.processTransaction(transaction = tx,
                                                 blockHashOpt =
                                                   blockHashes.headOption)
        balance <- newTxWallet.getBalance()
        unconfirmedBalance <- newTxWallet.getUnconfirmedBalance()
      } yield {
        //balance doesn't have to exactly equal, as there was money in the
        //wallet before hand.
        assert(balance >= amt)
        assert(amt == unconfirmedBalance)
        newTxWallet
      }

      for {
        _ <- newTxWalletF
        rescanState <- wallet.rescanNeutrinoWallet(startOpt = None,
                                                   endOpt = None,
                                                   addressBatchSize =
                                                     DEFAULT_ADDR_BATCH_SIZE,
                                                   useCreationTime = true,
                                                   force = false)
        _ <- {
          rescanState match {
            case started: RescanState.RescanStarted => started.blocksMatchedF
            case _: RescanState                     => Future.unit
          }
        }
        balance <- wallet.getBalance()
        unconfirmedBalance <- wallet.getUnconfirmedBalance()
      } yield {
        assert(balance == Bitcoins(7))
        assert(unconfirmedBalance == Bitcoins(1))
      }
  }

  it must "NOT discover funds that happened OUTSIDE of a certain range of block hashes" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet

      val initBalanceF = wallet.getBalance()

      //find the first block a utxo was created in
      val utxosF = wallet.listUtxos()
      val oldestHeightF = for {
        utxos <- utxosF
        blockhashes <- wallet
          .findByTxIds(utxos.map(_.txid))
          .map(_.flatMap(_.blockHashOpt))
        heights <- FutureUtil.sequentially(blockhashes) { hash =>
          wallet.chainQueryApi.getBlockHeight(hash)
        }
      } yield heights.min.get

      //ok now that we have the height of the oldest utxo, let's rescan up to then
      val rescanF = for {
        initBalance <- initBalanceF
        _ =
          assert(initBalance > CurrencyUnits.zero,
                 s"Cannot run rescan test if our init wallet balance is zero!")
        oldestUtxoHeight <- oldestHeightF
        end = Some(BlockStamp.BlockHeight(oldestUtxoHeight - 1))
        rescanState <- wallet.rescanNeutrinoWallet(startOpt =
                                                     BlockStamp.height0Opt,
                                                   endOpt = end,
                                                   addressBatchSize =
                                                     DEFAULT_ADDR_BATCH_SIZE,
                                                   useCreationTime = false,
                                                   force = false)
        _ <- {
          rescanState match {
            case started: RescanState.RescanStarted => started.blocksMatchedF
            case _: RescanState                     => Future.unit
          }
        }
        balanceAfterRescan <- wallet.getBalance()
      } yield {
        assert(balanceAfterRescan == CurrencyUnits.zero)
      }

      rescanF
  }

  it must "acknowledge that a rescan is already in progress" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet
      //do these in parallel on purpose to simulate multiple threads calling rescan
      val startF = wallet.rescanNeutrinoWallet(startOpt = None,
                                               endOpt = None,
                                               addressBatchSize =
                                                 DEFAULT_ADDR_BATCH_SIZE,
                                               useCreationTime = true,
                                               force = false)

      //slight delay to make sure other rescan is started
      val alreadyStartedF =
        AsyncUtil.nonBlockingSleep(10.millis).flatMap { _ =>
          wallet.rescanNeutrinoWallet(startOpt = None,
                                      endOpt = None,
                                      addressBatchSize =
                                        DEFAULT_ADDR_BATCH_SIZE,
                                      useCreationTime = true,
                                      force = false)
        }
      for {
        start <- startF
        _ = assert(start.isInstanceOf[RescanState.RescanStarted])
        //try another one
        alreadyStarted <- alreadyStartedF
        _ <- start.asInstanceOf[RescanState.RescanStarted].stop()
      } yield {
        assert(alreadyStarted == RescanState.RescanAlreadyStarted)
      }
  }

  it must "still receive payments to addresses generated pre-rescan" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet
      val bitcoind = fixture.bitcoind
      val addressNoFundsF = wallet.getNewAddress()

      //start a rescan without sending payment to that address
      for {
        address <- addressNoFundsF
        _ <- wallet.rescanNeutrinoWallet(startOpt = None,
                                         endOpt = None,
                                         addressBatchSize = 10,
                                         useCreationTime = true,
                                         force = false)

        usedAddresses <- wallet.listFundedAddresses()

        _ = assert(!usedAddresses.exists(_._1.address == address),
                   s"Address should not be used! address=$address")
        //now send a payment to our wallet
        hashes <- bitcoind.generateToAddress(1, address)
        block <- bitcoind.getBlockRaw(hashes.head)
        _ <- wallet.processBlock(block)
        fundedAddresses <- wallet.listFundedAddresses()
        utxos <- wallet.listUtxos(TxoState.ImmatureCoinbase)
      } yield {
        //note 25 bitcoin reward from coinbase tx here
        //if we we move this test case in the future it may need to change
        val expectedOutput =
          TransactionOutput(Bitcoins(25), address.scriptPubKey)
        assert(utxos.exists(_.output == expectedOutput),
               s"Balance must show up on utxos")
        val addressExists = fundedAddresses.exists(_._1.address == address)
        assert(addressExists)
      }
  }

  it must "discover funds after rescanning twice" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet
      val bitcoind = fixture.bitcoind
      val amt = Bitcoins.one
      for {
        _ <- wallet.rescanNeutrinoWallet(startOpt = None,
                                         endOpt = None,
                                         addressBatchSize = 10,
                                         useCreationTime = true,
                                         force = false)
        addressNoFunds <- wallet.getNewChangeAddress()
        //rescan again
        _ <- wallet.rescanNeutrinoWallet(startOpt = None,
                                         endOpt = None,
                                         addressBatchSize = 10,
                                         useCreationTime = true,
                                         force = false)
        txid <- bitcoind.sendToAddress(addressNoFunds, amt)
        tx <- bitcoind.getRawTransactionRaw(txid)
        _ <- wallet.processTransaction(tx, None)
        unconfirmedBalance <- wallet.getUnconfirmedBalance()
      } yield {
        assert(unconfirmedBalance == amt)
      }
  }

  it must "set the rescan flag when a rescan starts and then unset it when the rescan is done" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet

      for {
        isRescanning1 <- wallet.isRescanning()
        _ = assert(!isRescanning1,
                   "Cannot be rescanning before we started the test")
        //start the rescan
        state <- wallet.rescanNeutrinoWallet(startOpt = None,
                                             endOpt = None,
                                             addressBatchSize = 10,
                                             useCreationTime = true,
                                             force = false)
        isRescanning2 <- wallet.isRescanning()
        _ = assert(isRescanning2,
                   s"Rescan flag must be set after starting a rescan")
        _ <- RescanState.awaitRescanDone(state)
        _ <- AsyncUtil.nonBlockingSleep(
          1.second
        ) //extra buffer to avoid race condition
        isRescanning3 <- wallet.isRescanning()
      } yield {
        assert(!isRescanning3)
      }
  }

  it must "set the rescan flag when a rescan starts and then unset it when the rescan has an exception occur" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet

      for {
        isRescanning1 <- wallet.isRescanning()
        _ = assert(!isRescanning1,
                   "Cannot be rescanning before we started the test")
        //start the rescan
        state <- wallet.rescanNeutrinoWallet(startOpt = None,
                                             endOpt = None,
                                             addressBatchSize = 10,
                                             useCreationTime = true,
                                             force = false)
        isRescanning2 <- wallet.isRescanning()
        _ = assert(isRescanning2,
                   s"Rescan flag must be set after starting a rescan")
        _ = state match {
          case started: RescanState.RescanStarted =>
            started.fail(
              new RuntimeException(
                "Purposefully terminate rescan early for test"))
          case RescanState.RescanDone | RescanState.RescanAlreadyStarted =>
            fail(s"Rescan must be started")
        }
        _ <- AsyncUtil.nonBlockingSleep(
          1.second
        ) //extra buffer to avoid race condition
        isRescanning3 <- wallet.isRescanning()
      } yield {
        assert(!isRescanning3)
      }
  }

  it must "discover payments that occur in the same tx but are discovered during different batches during rescans" in {
    fixture: WalletWithBitcoindRpc =>
      val wallet = fixture.wallet
      val bitcoind = fixture.bitcoind

      val amt = Bitcoins.one
      val expectedBalance = amt * 2
      val numBlocks = 1
      val initBalanceF = wallet.getBalance()

      val defaultAccountF = wallet.getDefaultAccount()
      //send funds to a fresh wallet address
      val addr1F = wallet.getNewAddress()
      val addr2F = wallet.getNewAddress()
      val bitcoindAddrF = bitcoind.getNewAddress
      val balanceAfterPayment1F = for {
        addr1 <- addr1F
        addr2 <- addr2F
        _ <- initBalanceF
        //its important that both of these payments are sent in one tx
        //so that we check that we can discover payments in the same tx
        //across different rescan batches
        sendManyMap = Map(addr1 -> amt, addr2 -> amt)
        txid <- bitcoind.sendMany(sendManyMap)
        tx <- bitcoind.getRawTransactionRaw(txid)
        bitcoindAddr <- bitcoindAddrF
        blockHashes <-
          bitcoind.generateToAddress(blocks = numBlocks, address = bitcoindAddr)
        newTxWallet <- wallet.processTransaction(transaction = tx,
                                                 blockHashOpt =
                                                   blockHashes.headOption)
        balance <- newTxWallet.getBalance()
        unconfirmedBalance <- newTxWallet.getUnconfirmedBalance()
      } yield {
        //balance doesn't have to exactly equal, as there was money in the
        //wallet before hand.
        assert(balance >= expectedBalance)
        assert(expectedBalance == unconfirmedBalance)
        balance
      }

      for {
        _ <- initBalanceF
        balanceAfterPayment1 <- balanceAfterPayment1F

        account <- defaultAccountF
        txIds <-
          wallet
            .listUtxos(account.hdAccount)
            .map(_.map(_.txid))
        _ <- wallet
          .findByTxIds(txIds)
          .map(_.flatMap(_.blockHashOpt))

        _ <- wallet.clearAllUtxos()
        _ <- wallet.clearAllAddresses()
        balanceAfterClear <- wallet.getBalance()
        rescanState <- wallet.fullRescanNeutrinoWallet(1, true)
        _ <- RescanState.awaitRescanDone(rescanState)
        _ <- AsyncUtil.awaitConditionF(
          () => wallet.getBalance().map(_ == balanceAfterPayment1),
          maxTries = 100)
        balanceAfterRescan <- wallet.getBalance()
      } yield {
        assert(balanceAfterClear == CurrencyUnits.zero)
        assert(balanceAfterRescan == balanceAfterPayment1)
      }
  }

  it must "generate addresses for a rescan that are both external and change addresses" in {
    fixture =>
      val wallet = fixture.wallet

      val rescanF = for {
        //need to clear all utxos / addresses to test this
        //otherwise the wallet will see we already have addresses and not generate
        //a fresh pool of addresses
        _ <- wallet.clearAllUtxos()
        _ <- wallet.clearAllAddresses()
        rescanState <- wallet.fullRescanNeutrinoWallet(DEFAULT_ADDR_BATCH_SIZE)
        _ = assert(rescanState.isInstanceOf[RescanState.RescanStarted])
        _ <- RescanState.awaitRescanDone(rescanState)
        addresses <- wallet.listAddresses()
      } yield {
        assert(addresses.exists(_.isChange))
        assert(addresses.exists(!_.isChange))
      }

      rescanF
  }

}
