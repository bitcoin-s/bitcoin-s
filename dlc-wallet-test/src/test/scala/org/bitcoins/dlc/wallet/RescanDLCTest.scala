package org.bitcoins.dlc.wallet

import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.protocol.BlockStamp.BlockHash
import org.bitcoins.core.protocol.dlc.models.{
  DLCState,
  DisjointUnionContractInfo,
  SingleContractInfo
}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.BitcoindRpcBackendUtil
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.{DLCWalletUtil, DualWalletTestCachedBitcoind}
import org.bitcoins.wallet.config.WalletAppConfig
import org.scalatest.FutureOutcome

import java.nio.file.Files

class RescanDLCTest extends DualWalletTestCachedBitcoind {

  type FixtureParam =
    (InitializedDLCWallet, InitializedDLCWallet, BitcoindRpcClient)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test, DLCWalletUtil.sampleContractOraclePair)
  }

  behavior of "DLCWallet"

  it must "properly rescan after a DLC as initiator" in { params =>
    val walletA = params._1
    val wallet = walletA.wallet
    val walletB = params._2
    val bitcoind = params._3

    for {
      contractId <- getContractId(wallet)
      status <- getDLCStatus(wallet)
      (sig, _) = {
        status.contractInfo match {
          case single: SingleContractInfo =>
            DLCWalletUtil.getSigs(single)
          case disjoint: DisjointUnionContractInfo =>
            sys.error(
              s"Cannot retrieve sigs for disjoint union contract, got=$disjoint")
        }
      }
      func = (wallet: DLCWallet) =>
        wallet.executeDLC(contractId, sig).map(_.get)

      result <- dlcExecutionTest(wallets = (walletA, walletB),
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      Vector(hash) <- bitcoind.getNewAddress.flatMap(
        bitcoind.generateToAddress(1, _))

      _ <- wallet.rescanNeutrinoWallet(startOpt = None,
                                       endOpt = Some(BlockHash(hash)),
                                       addressBatchSize = 20,
                                       useCreationTime = false)

      postStatus <- getDLCStatus(wallet)
    } yield assert(postStatus.state == DLCState.Claimed)
  }

  it must "properly rescan after a DLC as recipient" in { params =>
    val walletA = params._1
    val walletB = params._2
    val wallet = walletB.wallet
    val bitcoind = params._3

    for {
      contractId <- getContractId(wallet)
      status <- getDLCStatus(wallet)
      (sig, _) = {
        status.contractInfo match {
          case single: SingleContractInfo =>
            DLCWalletUtil.getSigs(single)
          case disjoint: DisjointUnionContractInfo =>
            sys.error(
              s"Cannot retrieve sigs for disjoint union contract, got=$disjoint")
        }
      }
      func = (wallet: DLCWallet) =>
        wallet.executeDLC(contractId, sig).map(_.get)

      result <- dlcExecutionTest(wallets = (walletA, walletB),
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      Vector(hash) <- bitcoind.getNewAddress.flatMap(
        bitcoind.generateToAddress(1, _))

      _ <- wallet.rescanNeutrinoWallet(startOpt = None,
                                       endOpt = Some(BlockHash(hash)),
                                       addressBatchSize = 20,
                                       useCreationTime = false)

      postStatus <- getDLCStatus(wallet)
    } yield assert(postStatus.state == DLCState.RemoteClaimed)
  }

  it must "properly rescan when a dlc is remotely claimed" in { params =>
    //see: https://github.com/bitcoin-s/bitcoin-s/issues/4227
    val walletA = params._1
    val walletB = params._2
    val wallet = walletB.wallet
    val bitcoind = params._3
    val walletConfig = wallet.walletConfig
    val walletDb = walletConfig.datadir
      .resolve("walletdb.sqlite")
    for {
      contractId <- getContractId(wallet)
      status <- getDLCStatus(wallet)
      (sig, _) = {
        status.contractInfo match {
          case single: SingleContractInfo =>
            DLCWalletUtil.getSigs(single)
          case disjoint: DisjointUnionContractInfo =>
            sys.error(
              s"Cannot retrieve sigs for disjoint union contract, got=$disjoint")
        }
      }
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)
      result <- dlcExecutionTest(wallets = (walletA, walletB),
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)
      _ = assert(result)
      addr <- bitcoind.getNewAddress

      //confirm the closing tx in a block
      _ <- bitcoind.generateToAddress(1, addr)

      dlcsBeforeRescan <- wallet.listDLCs()
      _ = assert(dlcsBeforeRescan.exists(_.state == DLCState.RemoteClaimed))
      //destroy the wallet
      _ <- walletConfig.stop()
      _ = assert(Files.exists(walletDb))
      _ = Files.delete(walletDb)
      _ = assert(!Files.exists(walletDb))
      wAppConfig = WalletAppConfig(baseDatadir = walletConfig.baseDatadir,
                                   Vector.empty)(system.dispatcher)
      _ <- wAppConfig.start()
      //start it again with a fresh database
      wBadNodeApi <- DLCAppConfig(walletConfig.baseDatadir, Vector.empty)
        .createDLCWallet(
          wallet.nodeApi,
          wallet.chainQueryApi,
          feeRateApi = wallet.feeRateApi)(wAppConfig, system.dispatcher)
      w = BitcoindRpcBackendUtil.createDLCWalletWithBitcoindCallbacks(
        bitcoind = bitcoind,
        wallet = wBadNodeApi,
        chainCallbacksOpt = None)
      txCount <- w.transactionDAO.count()
      _ = assert(txCount == 0) //make sure we have txs in db
      //need to do a rescan from walletB
      _ <- w.rescanNeutrinoWallet(None, None, 5, false)
      balanceAfterRescan <- w.getBalance()
      _ = assert(balanceAfterRescan != CurrencyUnits.zero)
      dlcsAfterRescan <- w.listDLCs()
      _ <- wAppConfig.stop()
    } yield {
      assert(dlcsAfterRescan.exists(_.state == DLCState.RemoteClaimed))
    }
  }
}
