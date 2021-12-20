package org.bitcoins.dlc.wallet

import org.bitcoins.core.protocol.dlc.models.{
  DLCState,
  DLCStatus,
  DisjointUnionContractInfo,
  SingleContractInfo
}
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.bitcoins.testkitcore.gen.CryptoGenerators
import org.scalatest.FutureOutcome

import scala.concurrent.{Future, Promise}

class DLCWalletCallbackTest extends BitcoinSDualWalletTest {
  override type FixtureParam = (FundedDLCWallet, FundedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualFundedDLCWallets(test)
  }

  behavior of "DLCWalletCallbackTest"

  it must "verify OnDLCStateChange are executed" in { wallets =>
    val walletA: DLCWallet = wallets._1.wallet
    val walletB: DLCWallet = wallets._2.wallet
    val offerP: Promise[DLCStatus] = Promise()
    val acceptP: Promise[DLCStatus] = Promise()
    val signedP: Promise[DLCStatus] = Promise()
    val broadcastP: Promise[DLCStatus] = Promise()

    //not implemented yet
    val confirmedP: Promise[DLCStatus] = Promise()

    val claimedP: Promise[DLCStatus] = Promise()
    val remoteClaimedP: Promise[DLCStatus] = Promise()

    val walletACallback: OnDLCStateChange = { case status: DLCStatus =>
      status.state match {
        case DLCState.Offered =>
          Future.successful(offerP.success(status))
        case DLCState.Signed =>
          Future.successful(signedP.success(status))
        case DLCState.Confirmed =>
          Future.successful(confirmedP.success(status))
        case DLCState.Claimed =>
          Future.successful(claimedP.success(status))
        case DLCState.Broadcasted =>
          //ignore broadcast from this wallet
          Future.unit
        case x @ (DLCState.Accepted | DLCState.Confirmed |
            DLCState.RemoteClaimed | DLCState.Refunded) =>
          sys.error(s"Shouldn't receive state=$x for callback")
      }

    }

    val walletBCallback: OnDLCStateChange = { case status: DLCStatus =>
      status.state match {
        case DLCState.Accepted =>
          Future.successful(acceptP.success(status))
        case DLCState.Broadcasted =>
          Future.successful(broadcastP.success(status))
        case DLCState.RemoteClaimed =>
          Future.successful(remoteClaimedP.success(status))
        case x @ (DLCState.Offered | DLCState.Signed) =>
          sys.error(s"Shouldn't receive state=$x for callback")
        case DLCState.Confirmed | DLCState.Claimed | DLCState.Refunded =>
          //do nothing, we are doing assertions for these on walletACallback
          Future.unit
      }

    }

    val walletACallbacks = DLCWalletCallbacks.onDLCStateChange(walletACallback)

    val walletBCallbacks = DLCWalletCallbacks.onDLCStateChange(walletBCallback)

    walletA.dlcConfig.addCallbacks(walletACallbacks)
    walletB.dlcConfig.addCallbacks(walletBCallbacks)

    //run init DLC and make sure we get the callback hit

    val initF = DLCWalletUtil.initDLC(wallets._1,
                                      wallets._2,
                                      DLCWalletUtil.sampleContractInfo)
    val executeF = for {
      _ <- initF
      contractId <- DLCWalletUtil.getContractId(wallets._1.wallet)
      fundingTx <- walletA.getDLCFundingTx(contractId)
      _ <- walletA.processTransaction(
        transaction = fundingTx,
        blockHashOpt = Some(CryptoGenerators.doubleSha256DigestBE.sample.get))
      sigs = {
        DLCWalletUtil.sampleContractInfo match {
          case single: SingleContractInfo =>
            DLCWalletUtil.getSigs(single)
          case disjoint: DisjointUnionContractInfo =>
            sys.error(
              s"Cannot retrieve sigs for disjoint union contract, got=$disjoint")
        }
      }
      transaction <- walletA.executeDLC(contractId, sigs._1)
      _ <- walletB.processTransaction(transaction, None)
    } yield ()

    for {
      _ <- initF
      offer <- offerP.future
      accept <- acceptP.future
      sign <- signedP.future
      broadcast <- broadcastP.future
      confirmed <- confirmedP.future
      _ <- executeF
      claimed <- claimedP.future
      remoteClaimed <- remoteClaimedP.future
    } yield {
      assert(offer.state == DLCState.Offered)
      assert(accept.state == DLCState.Accepted)
      assert(sign.state == DLCState.Signed)
      assert(broadcast.state == DLCState.Broadcasted)
      assert(confirmed.state == DLCState.Confirmed)
      assert(claimed.state == DLCState.Claimed)
      assert(remoteClaimed.state == DLCState.RemoteClaimed)
    }
  }
}
