package org.bitcoins.wallet

import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.core.wallet.utxo.TxoState._
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.WalletTestUtil._
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
      //it must stay reserved if we are receiving confirmations
      assert(wallet.updateReceivedTxoWithConfs(reserved, 1) == reserved)

      //it must stay immature coinbase if we don't have > 101 confirmations
      assert(
        wallet.updateReceivedTxoWithConfs(immatureCoinbase,
                                          10) == immatureCoinbase)
      assert(
        wallet.updateReceivedTxoWithConfs(immatureCoinbase,
                                          101) == confReceived)

      //we must stay pending confirmations received with only 1 confirmation
      assert(
        wallet.updateReceivedTxoWithConfs(pendingConfReceived,
                                          1) == pendingConfReceived)

      //must be able to go back to broadcast received if we don't have confirmations (re-org scenario)
      assert(
        wallet
          .updateReceivedTxoWithConfs(pendingConfReceived, 0)
          .state == TxoState.BroadcastReceived)

      //it must transition from DoesNotExist -> PendingConfirmationsReceived
      assert(
        wallet.updateReceivedTxoWithConfs(dne, 1) == dne.copyWithState(
          TxoState.PendingConfirmationsReceived))

      //transition from TxoState.ConfirmedReceived -> TxoState.PendingConfirmationsReceived (reorg scenario)
      assert(
        wallet.updateReceivedTxoWithConfs(confReceived, 1) == confReceived
          .copyWithState(PendingConfirmationsReceived))

      //it must stay TxoState.ConfirmedReceived if we keep receiving confirmations
      assert(
        wallet.updateReceivedTxoWithConfs(confReceived,
                                          requiredConfs) == confReceived)

      //it must stay broadcast received with 0 confirmations
      val broadcastReceived = utxo
        .copyWithState(TxoState.BroadcastReceived)
      assert(
        wallet.updateReceivedTxoWithConfs(broadcastReceived,
                                          0) == broadcastReceived)
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
        wallet
          .updateSpentTxoWithConfs(withSpendingTxId, 0)
          .state == TxoState.BroadcastSpent)

      //it must transition from reserved to spent
      assert(
        wallet
          .updateSpentTxoWithConfs(withSpendingTxId, 1)
          .state == TxoState.PendingConfirmationsSpent)

      //cannot spend an immature coinbase output
      assertThrows[RuntimeException] {
        //cannot have utxo spent from coinbase before 101 blocks
        wallet.updateSpentTxoWithConfs(immatureCoinbase, 100)
      }

      val pendingConfReceivedWithTxId = pendingConfReceived
        .copyWithSpendingTxId(DoubleSha256DigestBE.empty)

      val expectedConfSpent = pendingConfReceived
        .copyWithSpendingTxId(DoubleSha256DigestBE.empty)
        .copyWithState(TxoState.ConfirmedSpent)

      val updated =
        wallet.updateSpentTxoWithConfs(pendingConfReceivedWithTxId,
                                       requiredConfs)
      //it must transition from Pending Confirmation Received -> Pending Confirmation Spent
      assert(updated == expectedConfSpent)

      //must stay at pending confirmations spent with only 1 confirmation
      assert(
        wallet.updateSpentTxoWithConfs(pendingConfSpent, 1) == pendingConfSpent)

      //must transition from PendingConfirmationsSpent -> ConfirmedSpent
      assert(
        wallet.updateSpentTxoWithConfs(pendingConfSpent,
                                       requiredConfs) == confSpent)

      val expectedDNEWithTxId =
        dne.copyWithSpendingTxId(DoubleSha256DigestBE.empty)
      val expectedDNEConfirmedSpent =
        expectedDNEWithTxId.copyWithState(TxoState.ConfirmedSpent)
      //transition from TxoState.DoesNotExist -> TxoState.ConfirmedSpent
      assert(
        wallet.updateSpentTxoWithConfs(
          expectedDNEWithTxId,
          requiredConfs) == expectedDNEConfirmedSpent)

      //transition form TxoState.ConfirmedSpent -> TxoState.PendingConfirmationSpent (reorg scenario)
      assert(
        wallet.updateSpentTxoWithConfs(confSpent, 1) == confSpent.copyWithState(
          PendingConfirmationsSpent))

      //stay confirmed if we are already confirmed
      assert(
        wallet.updateSpentTxoWithConfs(confSpent, requiredConfs) == confSpent)

      val expectedConfReceivedWithTxid =
        confReceived.copyWithSpendingTxId(DoubleSha256DigestBE.empty)

      //TxoState.ConfirmedReceived -> TxoState.ConfirmedSpent
      assert(
        wallet.updateSpentTxoWithConfs(
          expectedConfReceivedWithTxid,
          requiredConfs) == expectedConfReceivedWithTxid.copyWithState(
          TxoState.ConfirmedSpent))

      //it must stay BroadcastSpent with 0 confirmations
      val broadcastSpent = utxo
        .copyWithSpendingTxId(DoubleSha256DigestBE.empty)
        .copyWithState(TxoState.BroadcastSpent)
      assert(
        wallet.updateSpentTxoWithConfs(broadcastSpent, 0) == broadcastSpent)
  }
}
