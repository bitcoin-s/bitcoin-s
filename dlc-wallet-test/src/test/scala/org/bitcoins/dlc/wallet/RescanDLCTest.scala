package org.bitcoins.dlc.wallet

import org.bitcoins.core.protocol.BlockStamp.BlockHash
import org.bitcoins.core.protocol.dlc.models.{
  DLCState,
  DisjointUnionContractInfo,
  SingleContractInfo
}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.{DLCWalletUtil, DualWalletTestCachedBitcoind}
import org.scalatest.FutureOutcome

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
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)

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
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)

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
}
