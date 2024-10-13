package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.commons.rpc.BitcoindWalletException
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.rpc.client.common._
import org.bitcoins.rpc.config.{BitcoindInstanceLocal, BitcoindInstanceRemote}
import org.bitcoins.rpc.util.{NodePair, RpcUtil}
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairNewest,
  BitcoindRpcTestUtil
}
import org.bitcoins.testkit.util.PekkoUtil
import org.scalatest.{FutureOutcome, Outcome}

import java.io.File
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/** These tests are all copied over from WalletRpcTest and changed to be for
  * multi-wallet
  */
class MultiWalletRpcTest extends BitcoindFixturesCachedPairNewest {

  val walletName = "other"

  var password = "password"

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedSetupClientsF
      futOutcome = with2BitcoindsCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  private lazy val cachedSetupClientsF: Future[NodePair[BitcoindRpcClient]] = {
    clientsF.flatMap(setupWalletClient)
  }

  /** We need to test bitcoin core's wallet specific features, so we need to set
    * that up
    */
  private def setupWalletClient(
      pair: NodePair[BitcoindRpcClient]
  ): Future[NodePair[BitcoindRpcClient]] = {
    val NodePair(client: BitcoindRpcClient, walletClient: BitcoindRpcClient) =
      pair
    for {
      _ <- walletClient.createWallet(walletName)
      _ <- walletClient.encryptWallet(password, walletName)
      _ <-
        walletClient
          .getNewAddress(walletName)
          .flatMap(walletClient.generateToAddress(101, _))
      _ <- client.createWallet(walletName)

      // Restart so wallet is encrypted
      _ <- walletClient.stop()
      _ <- RpcUtil.awaitServerShutdown(walletClient)
      // Very rarely we are prevented from starting the client again because Core
      // hasn't released its locks on the datadir. This is prevent that.
      _ <- PekkoUtil.nonBlockingSleep(1.second)
      started <- walletClient.start()
      _ <- walletClient.loadWallet(walletName)
      _ <- walletClient.loadWallet(BitcoindRpcClient.DEFAULT_WALLET_NAME)

      wallets <- walletClient.listWallets
      wallets2 <- client.listWallets
      _ = require(wallets.size == 2)
      _ = require(wallets2.size == 2)
    } yield NodePair[BitcoindRpcClient](client, started)
  }

  behavior of "WalletRpc"

  it must "setup correctly" in { nodePair =>
    val walletClient = nodePair.node2
    for {
      wallets <- walletClient.listWallets
    } yield assert(wallets.size == 2)
  }

  it must "fail when no wallet is set" in { nodePair =>
    val walletClient = nodePair.node2
    recoverToSucceededIf[BitcoindWalletException](for {
      _ <- walletClient.getBalance
    } yield ())
  }

  it must "get balance" in { nodePair =>
    val walletClient = nodePair.node2
    for {
      balance <- walletClient.getBalance(walletName)
    } yield {
      // Has one mature coinbase
      assert(balance == Bitcoins(25))
    }
  }

  it should "be able to backup the wallet" in { nodePair =>
    val walletClient = nodePair.node2
    val localInstance = walletClient.getDaemon match {
      case _: BitcoindInstanceRemote =>
        sys.error(s"Cannot use remote bitcoind instance in test cases")
      case local: BitcoindInstanceLocal =>
        local
    }
    val datadir = localInstance.datadir.getAbsolutePath
    for {
      _ <- walletClient.backupWallet(datadir + "/backup.dat", walletName)
    } yield {
      val file = new File(datadir + "/backup.dat")
      assert(file.exists)
      assert(file.isFile)
    }
  }

  it should "be able to lock and unlock the wallet" in { nodePair =>
    val walletClient = nodePair.node2
    for {
      _ <- walletClient.walletLock(walletName)
      _ <- walletClient.walletPassphrase(password, 1000, walletName)

      info <- walletClient.getWalletInfo(walletName)
      _ = assert(info.unlocked_until.nonEmpty)
      _ = assert(info.unlocked_until.get > 0)

      _ <- walletClient.walletLock(walletName)

      newInfo <- walletClient.getWalletInfo(walletName)
    } yield assert(newInfo.unlocked_until.contains(0))
  }

  it should "be able to get an address from bitcoind" in { nodePair =>
    val client = nodePair.node2
    val addrFuts =
      List(
        client.getNewAddress("bech32", AddressType.Bech32, walletName),
        client.getNewAddress("p2sh", AddressType.P2SHSegwit, walletName),
        client.getNewAddress("legacy", AddressType.Legacy, walletName)
      )
    Future
      .sequence(addrFuts)
      .map(_ => succeed)
  }

  it should "be able to get a new raw change address" in { nodePair =>
    val client = nodePair.node2
    val addrFuts =
      List(
        client.getRawChangeAddress(walletName),
        client.getRawChangeAddress(AddressType.Bech32, walletName),
        client.getRawChangeAddress(AddressType.P2SHSegwit, walletName),
        client.getRawChangeAddress(AddressType.Legacy, walletName)
      )

    Future.sequence(addrFuts).map(_ => succeed)
  }

  it should "be able to get the amount recieved by some address" in {
    nodePair =>
      val client = nodePair.node2
      for {
        address <- client.getNewAddress(walletName)
        amount <-
          client.getReceivedByAddress(address, walletName = walletName)
      } yield assert(amount == Bitcoins(0))
  }

  it should "be able to get the unconfirmed balance" in { nodePair =>
    val client = nodePair.node2
    for {
      balance <- client.getUnconfirmedBalance(walletName)
    } yield {
      assert(balance == Bitcoins(0))
    }
  }

  it should "be able to get the wallet info" in { nodePair =>
    val client = nodePair.node2
    for {
      info <- client.getWalletInfo(walletName)
    } yield {
      assert(info.balance.toBigDecimal > 0)
      assert(info.txcount > 0)
      assert(info.keypoolsize > 0)
      assert(info.unlocked_until.contains(0))
    }
  }

  it should "be able to change the wallet password" in { nodePair =>
    val walletClient = nodePair.node2
    val newPass = "new_password"

    for {
      _ <- walletClient.walletLock(walletName)
      _ <-
        walletClient.walletPassphraseChange(password, newPass, walletName)
      _ = password = newPass

      _ <- walletClient.walletPassphrase(password, 1000, walletName)
      info <- walletClient.getWalletInfo(walletName)
      _ <- walletClient.walletLock(walletName)
      newInfo <- walletClient.getWalletInfo(walletName)
    } yield {

      assert(info.unlocked_until.nonEmpty)
      assert(info.unlocked_until.get > 0)
      assert(newInfo.unlocked_until.contains(0))
    }
  }

  it should "be able to send to an address" in { nodePair =>
    val otherClient = nodePair.node1
    val client = nodePair.node2
    for {
      address <- otherClient.getNewAddress(walletName)
      _ <- client.walletPassphrase(password, 1000, walletName)
      txid <- client.sendToAddress(
        address,
        Bitcoins(1),
        walletName = walletName
      )
      transaction <-
        client.getTransaction(txid, walletName = walletName)
    } yield {
      assert(transaction.amount == Bitcoins(-1))
      assert(transaction.details.head.address.contains(address))
    }
  }

  it should "be able to send btc to many addresses" in { nodePair =>
    val otherClient = nodePair.node1
    val client = nodePair.node2
    for {
      address1 <- otherClient.getNewAddress(walletName)
      address2 <- otherClient.getNewAddress(walletName)
      _ <- client.walletPassphrase(password, 1000, walletName)
      txid <-
        client
          .sendMany(
            Map(address1 -> Bitcoins(1), address2 -> Bitcoins(2)),
            walletName = walletName
          )
      transaction <-
        client.getTransaction(txid, walletName = walletName)
    } yield {
      assert(transaction.amount == Bitcoins(-3))
      assert(transaction.details.exists(_.address.contains(address1)))
      assert(transaction.details.exists(_.address.contains(address2)))
    }
  }

  it should "be able to get the balance" in { nodePair =>
    val client = nodePair.node2
    for {
      balance <- client.getBalance(walletName)
      _ <-
        client
          .getNewAddress(walletName)
          .flatMap(client.generateToAddress(1, _))
      newBalance <- client.getBalance(walletName)
    } yield {
      assert(balance.toBigDecimal > 0)
      assert(balance.toBigDecimal < newBalance.toBigDecimal)
    }
  }

  it should "be able to set the tx fee" in { nodePair =>
    val client = nodePair.node2
    for {
      success <- client.setTxFee(Bitcoins(0.01), walletName)
      info <- client.getWalletInfo(walletName)
    } yield {
      assert(success)
      assert(info.paytxfee == SatoshisPerByte(Satoshis(1000)))
    }
  }

  it should "be able to sign a raw transaction with the wallet" in { nodePair =>
    val otherClient = nodePair.node1
    val client = nodePair.node2
    for {
      address <- otherClient.getNewAddress(walletName)
      transactionWithoutFunds <-
        client
          .createRawTransaction(Vector.empty, Map(address -> Bitcoins(1)))
      transactionResult <-
        client.fundRawTransaction(transactionWithoutFunds, walletName)
      transaction = transactionResult.hex
      singedTx <-
        client
          .signRawTransactionWithWallet(transaction, walletName)
          .map(_.hex)

      // Will throw error if invalid
      _ <- client.sendRawTransaction(singedTx)
    } yield {
      assert(transaction.inputs.length == 1)
      assert(
        transaction.outputs.contains(
          TransactionOutput(Bitcoins(1), address.scriptPubKey)
        )
      )
    }
  }

  def startClient(client: BitcoindRpcClient): Future[Unit] = {
    BitcoindRpcTestUtil.startServers(Vector(client))
  }
}
