package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{InternalAddressTag, StorageLocationTag}
import org.bitcoins.server.BitcoindRpcBackendUtil
import org.bitcoins.testkit.util.PekkoUtil
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTest,
  WalletTestUtil,
  WalletWithBitcoindRpc
}
import org.bitcoins.wallet.models.IncomingTransactionDAO
import org.scalatest.FutureOutcome

import scala.concurrent.duration.DurationInt

class AddressTagIntegrationTest extends BitcoinSWalletTest {

  override type FixtureParam = WalletWithBitcoindRpc

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWalletAndBitcoind(test)(getFreshWalletAppConfig)

  behavior of "Address Tag - integration test"

  val exampleTag: InternalAddressTag = StorageLocationTag.HotStorage

  it should "correctly keep tagged utxos separated" in { walletWithBitcoind =>
    val wallet = walletWithBitcoind.wallet
    val bitcoind = walletWithBitcoind.bitcoind
    val walletConfig = walletWithBitcoind.walletConfig
    // the amount we're receiving from bitcoind
    val valueFromBitcoind = Bitcoins.one

    // the amount we're sending to bitcoind
    val valueToBitcoind = Bitcoins(0.5)
    val incomingTxDAO =
      IncomingTransactionDAO()(system.dispatcher, walletConfig)
    for {
      addr <- wallet.getNewAddress()
      taggedAddr <- wallet.addressHandling.getNewAddress(Vector(exampleTag))
      txId <- bitcoind.sendMany(
        Map(addr -> valueFromBitcoind, taggedAddr -> valueFromBitcoind)
      )
      tx <- bitcoind.getRawTransactionRaw(txId)

      // before processing TX, wallet should be completely empty
      _ <- wallet.utxoHandling.getUtxos().map(utxos => assert(utxos.isEmpty))
      _ <- wallet.getBalance().map(confirmed => assert(confirmed == 0.bitcoin))
      _ <-
        wallet
          .getUnconfirmedBalance()
          .map(unconfirmed => assert(unconfirmed == 0.bitcoin))

      // after this, tx is unconfirmed in wallet
      _ <- wallet.transactionProcessing.processTransaction(tx, None)

      // we should now have one UTXO in the wallet
      // it should not be confirmed
      utxosPostAdd <- wallet.utxoHandling.getUtxos()
      _ = assert(utxosPostAdd.length == 2)
      _ <-
        wallet
          .getConfirmedBalance()
          .map(confirmed => assert(confirmed == 0.bitcoin))
      _ <-
        wallet
          .getUnconfirmedBalance()
          .map(unconfirmed => assert(unconfirmed == valueFromBitcoind * 2))
      incomingTx <- incomingTxDAO.findByTxId(tx.txIdBE)
      _ = assert(incomingTx.isDefined)
      _ = assert(incomingTx.get.incomingAmount == valueFromBitcoind * 2)

      taggedUtxosPostAdd <- wallet.utxoHandling.getUtxos(exampleTag)
      _ = assert(taggedUtxosPostAdd.length == 1)
      _ <-
        wallet.utxoHandling
          .getUnconfirmedBalance(exampleTag)
          .map(unconfirmed => assert(unconfirmed == valueFromBitcoind))

      feeRate <- wallet.getFeeRate()
      rawTxHelper <- bitcoind.getNewAddress.flatMap { addr =>
        val output = TransactionOutput(valueToBitcoind, addr.scriptPubKey)
        wallet.fundTxHandling
          .fundRawTransaction(
            destinations = Vector(output),
            feeRate = feeRate,
            fromTagOpt = Some(exampleTag),
            markAsReserved = true
          )
      }
      signedTx = rawTxHelper.signedTx
      _ <- wallet.transactionProcessing.processTransaction(signedTx, None)

      utxos <- wallet.utxoHandling.getUtxos()
      balancePostSend <- wallet.getBalance()
      tagBalancePostSend <- wallet.utxoHandling.getBalance(exampleTag)
    } yield {
      // One change one external
      assert(utxos.size == 2)
      assert(
        utxos.exists(_.privKeyPath.chain.chainType == HDChainType.External)
      )
      assert(utxos.exists(_.privKeyPath.chain.chainType == HDChainType.Change))

      // untagged balance should be untouched
      assert(balancePostSend - tagBalancePostSend == valueFromBitcoind.satoshis)

      // change UTXO should be smaller than what we had, but still have money in it
      assert(tagBalancePostSend > 0.sats)
      assert(tagBalancePostSend < valueFromBitcoind)
      val utxoInfos = rawTxHelper.scriptSigParams
      val feePaid =
        utxoInfos.map(_.output.value).sum - signedTx.outputs.map(_.value).sum
      assert(
        WalletTestUtil.isCloseEnough(
          tagBalancePostSend,
          valueFromBitcoind - valueToBitcoind,
          delta = feePaid
        )
      )
    }
  }

  it must "process a tagged tx correctly when we broadcast it and receive it in a block" in {
    walletWithBitcoind =>
      // see: https://github.com/bitcoin-s/bitcoin-s/issues/4238
      val WalletWithBitcoindRpc(wallet, bitcoind, _) = walletWithBitcoind

      val bitcoindAddrF = bitcoind.getNewAddress
      val walletAddr1F = wallet.getNewAddress()
      val taggedAddrF = wallet.addressHandling.getNewAddress(Vector(exampleTag))
      for {
        walletAddr1 <- walletAddr1F
        txid <- bitcoind.sendToAddress(walletAddr1, Bitcoins.two)
        tx <- bitcoind.getRawTransaction(txid)
        _ <- wallet.transactionProcessing.processTransaction(tx.hex, None)
        taggedAddress <- taggedAddrF
        tx <- wallet.sendFundsHandling.sendToAddress(
          taggedAddress,
          Satoshis(100000),
          SatoshisPerVirtualByte.one,
          Vector(exampleTag)
        )
        _ <- wallet.transactionProcessing.processTransaction(tx, None)
        _ <- bitcoind.sendRawTransaction(tx)
        bitcoindAddr <- bitcoindAddrF
        _ <- PekkoUtil.nonBlockingSleep(1.second)
        _ <- bitcoind.generateToAddress(1, bitcoindAddr)
        _ <- BitcoindRpcBackendUtil.syncWalletToBitcoind(bitcoind, wallet, None)
      } yield succeed
  }
}
