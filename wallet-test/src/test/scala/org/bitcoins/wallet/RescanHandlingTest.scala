package org.bitcoins.wallet

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits, Satoshis}
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTest,
  WalletWithBitcoind,
  WalletWithBitcoindV19
}
import org.scalatest.FutureOutcome

import scala.concurrent.Future

class RescanHandlingTest extends BitcoinSWalletTest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = WalletWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWalletAndBitcoindV19(test, getBIP39PasswordOpt())
  }

  behavior of "Wallet rescans"

  it must "properly clear utxos and address for an account" in {
    fixture: WalletWithBitcoind =>
      val wallet = fixture.wallet

      for {
        accountDb <- wallet.getDefaultAccount()
        account = accountDb.hdAccount
        utxos <- wallet.spendingInfoDAO.findAllForAccount(account)
        _ = assert(utxos.nonEmpty)

        addresses <- wallet.addressDAO.findAllForAccount(account)
        _ = assert(addresses.nonEmpty)

        _ <- wallet.clearUtxosAndAddresses(account)

        clearedUtxos <- wallet.spendingInfoDAO.findAllForAccount(account)
        clearedAddresses <- wallet.addressDAO.findAllForAccount(account)
      } yield {
        assert(clearedUtxos.isEmpty)
        assert(clearedAddresses.isEmpty)
      }
  }

  it must "properly clear all utxos and address" in {
    fixture: WalletWithBitcoind =>
      val wallet = fixture.wallet

      for {
        balance <- wallet.getBalance()
        _ = assert(balance != Satoshis.zero)
        utxos <- wallet.spendingInfoDAO.findAll()
        _ = assert(utxos.nonEmpty)

        addresses <- wallet.addressDAO.findAll()
        _ = assert(addresses.nonEmpty)

        _ <- wallet.clearAllUtxosAndAddresses()

        clearedUtxos <- wallet.spendingInfoDAO.findAll()
        clearedAddresses <- wallet.addressDAO.findAll()
      } yield {
        assert(clearedUtxos.isEmpty)
        assert(clearedAddresses.isEmpty)
      }
  }

  val DEFAULT_ADDR_BATCH_SIZE = 10
  it must "be able to discover funds that belong to the wallet using WalletApi.rescanNeutrinoWallet" in {
    fixture: WalletWithBitcoind =>
      val WalletWithBitcoindV19(wallet, _) = fixture

      val initBalanceF = wallet.getBalance()

      val rescanF = for {
        initBalance <- initBalanceF
        _ =
          assert(initBalance > CurrencyUnits.zero,
                 s"Cannot run rescan test if our init wallet balance is zero!")
        _ <- wallet.fullRescanNeutrinoWallet(DEFAULT_ADDR_BATCH_SIZE)
        balanceAfterRescan <- wallet.getBalance()
      } yield {
        assert(balanceAfterRescan == initBalance)
      }

      rescanF
  }

  it must "be able to discover funds that occurred within a certain range" in {
    fixture: WalletWithBitcoind =>
      val WalletWithBitcoindV19(wallet, bitcoind) = fixture

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
        assert(balance == unconfirmedBalance)
        newTxWallet
      }

      //let's clear the wallet and then do a rescan for the last numBlocks
      //that means the wallet should only contain the amt we just processed
      for {
        newTxWallet <- newTxWalletF
        initBlockHeight <- initBlockHeightF
        txInBlockHeight = initBlockHeight + numBlocks
        txInBlockHeightOpt = Some(BlockStamp.BlockHeight(txInBlockHeight))
        _ <- newTxWallet.clearAllUtxosAndAddresses()
        zeroBalance <- newTxWallet.getBalance()
        _ = assert(zeroBalance == Satoshis.zero)
        _ <- newTxWallet.rescanNeutrinoWallet(startOpt = txInBlockHeightOpt,
                                              endOpt = None,
                                              addressBatchSize =
                                                DEFAULT_ADDR_BATCH_SIZE,
                                              useCreationTime = false)
        balance <- newTxWallet.getBalance()
      } yield {
        assert(balance == amt)
      }
  }

  it must "be able to discover funds using multiple batches" in {
    fixture: WalletWithBitcoind =>
      val WalletWithBitcoindV19(wallet, bitcoind) = fixture

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
        assert(balance == unconfirmedBalance)
        newTxWallet
      }

      for {
        newTxWallet <- newTxWalletF

        account <- newTxWallet.getDefaultAccount()
        blocks <-
          newTxWallet.spendingInfoDAO
            .findAllForAccount(account.hdAccount)
            .map(_.flatMap(_.blockHash).distinct)

        _ <- newTxWallet.clearAllUtxosAndAddresses()
        scriptPubKeys <-
          1.to(10).foldLeft(Future.successful(Vector.empty[ScriptPubKey])) {
            (prevFuture, _) =>
              for {
                prev <- prevFuture
                address <- newTxWallet.getNewAddress(account)
                changeAddress <- newTxWallet.getNewChangeAddress(account)
              } yield prev :+ address.scriptPubKey :+ changeAddress.scriptPubKey
          }
        matches <- newTxWallet.getMatchingBlocks(scriptPubKeys,
                                                 None,
                                                 None,
                                                 batchSize = 1)
      } yield {
        assert(matches.size == blocks.size)
        assert(
          matches.forall(blockMatch => blocks.contains(blockMatch.blockHash)))
      }
  }

  it must "be able to discover funds that occurred from the wallet creation time" in {
    fixture: WalletWithBitcoind =>
      val WalletWithBitcoindV19(wallet, bitcoind) = fixture

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
        assert(balance == unconfirmedBalance)
        newTxWallet
      }

      for {
        newTxWallet <- newTxWalletF
        _ <- newTxWallet.rescanNeutrinoWallet(startOpt = None,
                                              endOpt = None,
                                              addressBatchSize =
                                                DEFAULT_ADDR_BATCH_SIZE,
                                              useCreationTime = true)
        balance <- newTxWallet.getBalance()
      } yield {
        assert(balance == Bitcoins(7))
      }
  }

  it must "NOT discover funds that happened OUTSIDE of a certain range of block hashes" in {
    fixture: WalletWithBitcoind =>
      val WalletWithBitcoindV19(wallet, _) = fixture

      val initBalanceF = wallet.getBalance()

      //find the first block a utxo was created in
      val utxosF = wallet.listUtxos()
      val oldestHeightF = for {
        utxos <- utxosF
        blockhashes = utxos.map(_.blockHash)
        heights <- FutureUtil.sequentially(blockhashes) { hash =>
          wallet.chainQueryApi.getBlockHeight(hash.get)
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
        _ <- wallet.rescanNeutrinoWallet(startOpt = BlockStamp.height0Opt,
                                         endOpt = end,
                                         addressBatchSize =
                                           DEFAULT_ADDR_BATCH_SIZE,
                                         useCreationTime = false)
        balanceAfterRescan <- wallet.getBalance()
      } yield {
        assert(balanceAfterRescan == CurrencyUnits.zero)
      }

      rescanF
  }

}
