package org.bitcoins.wallet

import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.core.wallet.utxo.TxoState._
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.WalletTestUtil._
import org.bitcoins.wallet.internal.UtxoHandling
import org.scalatest.FutureOutcome

class UTXOHandlingTest extends BitcoinSWalletTest {

  behavior of "UTXOHandling"

  override type FixtureParam = Wallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withNewWallet(test, getBIP39PasswordOpt())(getFreshWalletAppConfig)
  }

  val utxo = sampleSegwitUTXO(EmptyScriptPubKey,
                              state = TxoState.Reserved
  ) //state doesn't matter here
  val requiredConfs = 6
  val reserved = utxo.copyWithState(Reserved)

  val immatureCoinbase = utxo.copyWithState(ImmatureCoinbase)

  val confReceived = utxo
    .copyWithState(ConfirmedReceived)

  val pendingConfReceived = utxo.copyWithState(PendingConfirmationsReceived)

  it must "correct update receive txo state based on confirmations" in {
    wallet =>
      val _ = wallet
      //it must stay reserved if we are receiving confirmations
      assert(
        UtxoHandling.updateReceivedTxoWithConfs(reserved,
                                                1,
                                                requiredConfs) == reserved)

      //it must stay immature coinbase if we don't have > 101 confirmations
      assert(
        UtxoHandling.updateReceivedTxoWithConfs(
          immatureCoinbase,
          10,
          requiredConfs) == immatureCoinbase)
      assert(
        UtxoHandling.updateReceivedTxoWithConfs(immatureCoinbase,
                                                101,
                                                requiredConfs) == confReceived)

      //we must stay pending confirmations received with only 1 confirmation
      assert(
        UtxoHandling.updateReceivedTxoWithConfs(
          pendingConfReceived,
          1,
          requiredConfs) == pendingConfReceived)

      //must be able to go back to broadcast received if we don't have confirmations (re-org scenario)
      assert(
        UtxoHandling
          .updateReceivedTxoWithConfs(pendingConfReceived, 0, requiredConfs)
          .state == TxoState.BroadcastReceived)

      //transition from TxoState.ConfirmedReceived -> TxoState.PendingConfirmationsReceived (reorg scenario)
      assert(
        UtxoHandling.updateReceivedTxoWithConfs(confReceived,
                                                1,
                                                requiredConfs) == confReceived
          .copyWithState(PendingConfirmationsReceived))

      //it must stay TxoState.ConfirmedReceived if we keep receiving confirmations
      assert(
        UtxoHandling.updateReceivedTxoWithConfs(confReceived,
                                                requiredConfs,
                                                requiredConfs) == confReceived)

      //it must stay broadcast received with 0 confirmations
      val broadcastReceived = utxo
        .copyWithState(TxoState.BroadcastReceived)
      assert(
        UtxoHandling.updateReceivedTxoWithConfs(
          broadcastReceived,
          0,
          requiredConfs) == broadcastReceived)
  }

  it must "correctly update spent txo state based on confirmations" in {
    wallet =>
      assert(wallet.walletConfig.requiredConfirmations == requiredConfs)

      val spendingTxId = randomTXID
      val pendingConfSpent = utxo
        .copyWithSpendingTxId(spendingTxId)
        .copyWithState(PendingConfirmationsSpent)

      val confSpent = utxo
        .copyWithSpendingTxId(spendingTxId)
        .copyWithState(ConfirmedSpent)

      val withSpendingTxId =
        reserved.copyWithSpendingTxId(DoubleSha256DigestBE.empty)

      //it must transition from reserved to broacast spent
      assert(
        UtxoHandling
          .updateSpentTxoWithConfs(withSpendingTxId, 0, requiredConfs)
          .state == TxoState.BroadcastSpent)

      //it must transition from reserved to spent
      assert(
        UtxoHandling
          .updateSpentTxoWithConfs(withSpendingTxId, 1, requiredConfs)
          .state == TxoState.PendingConfirmationsSpent)

      //cannot spend an immature coinbase output
      assertThrows[RuntimeException] {
        //cannot have utxo spent from coinbase before 101 blocks
        UtxoHandling.updateSpentTxoWithConfs(immatureCoinbase,
                                             100,
                                             requiredConfs)
      }

      val pendingConfReceivedWithTxId = pendingConfReceived
        .copyWithSpendingTxId(DoubleSha256DigestBE.empty)

      val expectedConfSpent = pendingConfReceived
        .copyWithSpendingTxId(DoubleSha256DigestBE.empty)
        .copyWithState(TxoState.ConfirmedSpent)

      val updated =
        UtxoHandling.updateSpentTxoWithConfs(pendingConfReceivedWithTxId,
                                             requiredConfs,
                                             requiredConfs)
      //it must transition from Pending Confirmation Received -> Pending Confirmation Spent
      assert(updated == expectedConfSpent)

      //must stay at pending confirmations spent with only 1 confirmation
      assert(
        UtxoHandling.updateSpentTxoWithConfs(pendingConfSpent,
                                             1,
                                             requiredConfs) == pendingConfSpent)

      //must transition from PendingConfirmationsSpent -> ConfirmedSpent
      assert(
        UtxoHandling.updateSpentTxoWithConfs(pendingConfSpent,
                                             requiredConfs,
                                             requiredConfs) == confSpent)

      //transition form TxoState.ConfirmedSpent -> TxoState.PendingConfirmationSpent (reorg scenario)
      assert(
        UtxoHandling.updateSpentTxoWithConfs(
          confSpent,
          1,
          requiredConfs) == confSpent.copyWithState(PendingConfirmationsSpent))

      //stay confirmed if we are already confirmed
      assert(
        UtxoHandling.updateSpentTxoWithConfs(confSpent,
                                             requiredConfs,
                                             requiredConfs) == confSpent)

      val expectedConfReceivedWithTxid =
        confReceived.copyWithSpendingTxId(DoubleSha256DigestBE.empty)

      //TxoState.ConfirmedReceived -> TxoState.ConfirmedSpent
      assert(
        UtxoHandling.updateSpentTxoWithConfs(
          expectedConfReceivedWithTxid,
          requiredConfs,
          requiredConfs) == expectedConfReceivedWithTxid.copyWithState(
          TxoState.ConfirmedSpent))

      //it must stay BroadcastSpent with 0 confirmations
      val broadcastSpent = utxo
        .copyWithSpendingTxId(DoubleSha256DigestBE.empty)
        .copyWithState(TxoState.BroadcastSpent)
      assert(
        UtxoHandling.updateSpentTxoWithConfs(broadcastSpent,
                                             0,
                                             requiredConfs) == broadcastSpent)
  }
}
