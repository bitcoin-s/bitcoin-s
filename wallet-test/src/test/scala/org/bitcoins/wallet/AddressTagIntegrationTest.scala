package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{InternalAddressTag, StorageLocationTag}
import org.bitcoins.server.BitcoindRpcBackendUtil
import org.bitcoins.testkit.util.AkkaUtil
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTest,
  WalletTestUtil,
  WalletWithBitcoindRpc
}
import org.scalatest.FutureOutcome

import scala.concurrent.duration.DurationInt

class AddressTagIntegrationTest extends BitcoinSWalletTest {

  override type FixtureParam = WalletWithBitcoindRpc

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWalletAndBitcoind(test)(getFreshWalletAppConfig)

  behavior of "Address Tag - integration test"

  val exampleTag: InternalAddressTag = StorageLocationTag.HotStorage

  it should "correctly keep tagged utxos separated" in { walletWithBitcoind =>
    val WalletWithBitcoindRpc(wallet, bitcoind) = walletWithBitcoind
    // the amount we're receiving from bitcoind
    val valueFromBitcoind = Bitcoins.one

    // the amount we're sending to bitcoind
    val valueToBitcoind = Bitcoins(0.5)

    for {
      addr <- wallet.getNewAddress()
      taggedAddr <- wallet.getNewAddress(Vector(exampleTag))
      txId <- bitcoind.sendMany(
        Map(addr -> valueFromBitcoind, taggedAddr -> valueFromBitcoind))
      tx <- bitcoind.getRawTransactionRaw(txId)

      // before processing TX, wallet should be completely empty
      _ <- wallet.listUtxos().map(utxos => assert(utxos.isEmpty))
      _ <- wallet.getBalance().map(confirmed => assert(confirmed == 0.bitcoin))
      _ <-
        wallet
          .getUnconfirmedBalance()
          .map(unconfirmed => assert(unconfirmed == 0.bitcoin))

      // after this, tx is unconfirmed in wallet
      _ <- wallet.processTransaction(tx, None)

      // we should now have one UTXO in the wallet
      // it should not be confirmed
      utxosPostAdd <- wallet.listUtxos()
      _ = assert(utxosPostAdd.length == 2)
      _ <-
        wallet
          .getConfirmedBalance()
          .map(confirmed => assert(confirmed == 0.bitcoin))
      _ <-
        wallet
          .getUnconfirmedBalance()
          .map(unconfirmed => assert(unconfirmed == valueFromBitcoind * 2))
      incomingTx <- wallet.incomingTxDAO.findByTxId(tx.txIdBE)
      _ = assert(incomingTx.isDefined)
      _ = assert(incomingTx.get.incomingAmount == valueFromBitcoind * 2)

      taggedUtxosPostAdd <- wallet.listUtxos(exampleTag)
      _ = assert(taggedUtxosPostAdd.length == 1)
      _ <-
        wallet
          .getUnconfirmedBalance(exampleTag)
          .map(unconfirmed => assert(unconfirmed == valueFromBitcoind))

      account <- wallet.getDefaultAccount()
      feeRate <- wallet.getFeeRate()
      rawTxHelper <- bitcoind.getNewAddress.flatMap { addr =>
        val output = TransactionOutput(valueToBitcoind, addr.scriptPubKey)
        wallet
          .fundRawTransactionInternal(destinations = Vector(output),
                                      feeRate = feeRate,
                                      fromAccount = account,
                                      fromTagOpt = Some(exampleTag),
                                      markAsReserved = true)
      }
      signedTx = rawTxHelper.signedTx
      _ <- wallet.processTransaction(signedTx, None)

      utxos <- wallet.listUtxos()
      balancePostSend <- wallet.getBalance()
      tagBalancePostSend <- wallet.getBalance(exampleTag)
    } yield {
      // One change one external
      assert(utxos.size == 2)
      assert(
        utxos.exists(_.privKeyPath.chain.chainType == HDChainType.External))
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
        WalletTestUtil.isCloseEnough(tagBalancePostSend,
                                     valueFromBitcoind - valueToBitcoind,
                                     delta = feePaid))
    }
  }

  it must "process a tagged tx correctly when we broadcast it and receive it in a block" in {
    walletWithBitcoind =>
      //see: https://github.com/bitcoin-s/bitcoin-s/issues/4238
      val WalletWithBitcoindRpc(wallet, bitcoind) = walletWithBitcoind

      val bitcoindAddrF = bitcoind.getNewAddress
      val walletAddr1F = wallet.getNewAddress()
      val taggedAddrF = wallet.getNewAddress(Vector(exampleTag))
      for {
        walletAddr1 <- walletAddr1F
        txid <- bitcoind.sendToAddress(walletAddr1, Bitcoins.two)
        tx <- bitcoind.getRawTransaction(txid)
        _ <- wallet.processTransaction(tx.hex, None)
        taggedAddress <- taggedAddrF
        tx <- wallet.sendToAddress(taggedAddress,
                                   Satoshis(100000),
                                   SatoshisPerVirtualByte.one,
                                   Vector(exampleTag))
        _ <- wallet.processTransaction(tx, None)
        _ <- bitcoind.sendRawTransaction(tx)
        bitcoindAddr <- bitcoindAddrF
        _ <- AkkaUtil.nonBlockingSleep(1.second)
        _ <- bitcoind.generateToAddress(1, bitcoindAddr)
        _ <- BitcoindRpcBackendUtil.syncWalletToBitcoind(bitcoind, wallet, None)
      } yield succeed
  }
}
