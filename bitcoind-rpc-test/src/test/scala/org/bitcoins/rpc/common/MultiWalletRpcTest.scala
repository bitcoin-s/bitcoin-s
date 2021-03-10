package org.bitcoins.rpc.common

import java.io.File
import java.util.Scanner
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.core.crypto.ECPrivateKeyUtil
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.crypto.{ECPrivateKey, ECPublicKey}
import org.bitcoins.rpc._
import org.bitcoins.rpc.client.common._
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.{AkkaUtil, BitcoindRpcTest}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/** These tests are all copied over from WalletRpcTest and changed to be for multi-wallet */
class MultiWalletRpcTest extends BitcoindRpcTest {

  val walletName = "other"

  var password = "password"

  lazy val clientsF: Future[
    (BitcoindRpcClient, BitcoindRpcClient, BitcoindRpcClient)] =
    BitcoindRpcTestUtil.createNodeTripleV19(clientAccum = clientAccum)

  lazy val walletClientF: Future[BitcoindRpcClient] = clientsF.flatMap {
    clients =>
      val walletClient =
        BitcoindRpcClient.withActorSystem(BitcoindRpcTestUtil.instance())
      clientAccum += walletClient

      for {
        _ <- startClient(walletClient)
        _ <- walletClient.createWallet(walletName)
        _ <- walletClient.encryptWallet(password, Some(walletName))
        _ <-
          walletClient
            .getNewAddress(Some(walletName))
            .flatMap(walletClient.generateToAddress(101, _))
        _ <- clients._1.createWallet(walletName)

        // Restart so wallet is encrypted
        _ <- walletClient.stop()
        _ <- RpcUtil.awaitServerShutdown(walletClient)
        // Very rarely we are prevented from starting the client again because Core
        // hasn't released its locks on the datadir. This is prevent that.
        _ <- AkkaUtil.nonBlockingSleep(1.second)
        _ <- walletClient.start()
        _ <- walletClient.loadWallet(walletName)

        wallets <- walletClient.listWallets
        wallets2 <- clients._1.listWallets
        _ = require(wallets.size == 2)
        _ = require(wallets2.size == 2)
      } yield walletClient
  }

  behavior of "WalletRpc"

  it must "setup correctly" in {
    for {
      walletClient <- walletClientF
      wallets <- walletClient.listWallets
    } yield assert(wallets.size == 2)
  }

  it must "fail when no wallet is set" in {
    recoverToSucceededIf[BitcoindWalletException](for {
      walletClient <- walletClientF
      _ <- walletClient.getBalance
    } yield ())
  }

  it must "get balance" in {
    for {
      walletClient <- walletClientF
      balance <- walletClient.getBalance(walletName)
    } yield {
      // Has one mature coinbase
      assert(balance == Bitcoins(50))
    }
  }

  it should "be able to backup the wallet" in {
    for {
      client <- walletClientF
      _ <- {
        val datadir = client.getDaemon.datadir.getAbsolutePath
        client.backupWallet(datadir + "/backup.dat", Some(walletName))
      }
    } yield {
      val datadir = client.getDaemon.datadir.getAbsolutePath
      val file = new File(datadir + "/backup.dat")
      assert(file.exists)
      assert(file.isFile)
    }
  }

  it should "be able to lock and unlock the wallet" in {
    for {
      walletClient <- walletClientF
      _ <- walletClient.walletLock(walletName)
      _ <- walletClient.walletPassphrase(password, 1000, Some(walletName))

      info <- walletClient.getWalletInfo(walletName)
      _ = assert(info.unlocked_until.nonEmpty)
      _ = assert(info.unlocked_until.get > 0)

      _ <- walletClient.walletLock(walletName)

      newInfo <- walletClient.getWalletInfo(walletName)
    } yield assert(newInfo.unlocked_until.contains(0))
  }

  it should "be able to get an address from bitcoind" in {
    for {
      client <- walletClientF
      _ <- {
        val addrFuts =
          List(
            client.getNewAddress("bech32", AddressType.Bech32, walletName),
            client.getNewAddress("p2sh", AddressType.P2SHSegwit, walletName),
            client.getNewAddress("legacy", AddressType.Legacy, walletName)
          )
        Future.sequence(addrFuts)
      }
    } yield succeed
  }

  it should "be able to get a new raw change address" in {
    for {
      client <- walletClientF
      _ <- {
        val addrFuts =
          List(
            client.getRawChangeAddress(walletName),
            client.getRawChangeAddress(AddressType.Bech32, walletName),
            client.getRawChangeAddress(AddressType.P2SHSegwit, walletName),
            client.getRawChangeAddress(AddressType.Legacy, walletName)
          )
        Future.sequence(addrFuts)
      }
    } yield succeed
  }

  it should "be able to get the amount recieved by some address" in {
    for {
      client <- walletClientF
      address <- client.getNewAddress(Some(walletName))
      amount <-
        client.getReceivedByAddress(address, walletNameOpt = Some(walletName))
    } yield assert(amount == Bitcoins(0))
  }

  it should "be able to get the unconfirmed balance" in {
    for {
      client <- walletClientF
      balance <- client.getUnconfirmedBalance(walletName)
    } yield {
      assert(balance == Bitcoins(0))
    }
  }

  it should "be able to get the wallet info" in {
    for {
      client <- walletClientF
      info <- client.getWalletInfo(walletName)
    } yield {
      assert(info.balance.toBigDecimal > 0)
      assert(info.txcount > 0)
      assert(info.keypoolsize > 0)
      assert(info.unlocked_until.contains(0))
    }
  }

  it should "be able to refill the keypool" in {
    for {
      client <- walletClientF
      _ <- client.walletPassphrase(password, 1000, Some(walletName))
      info <- client.getWalletInfo(walletName)
      _ <- client.keyPoolRefill(info.keypoolsize + 1, Some(walletName))
      newInfo <- client.getWalletInfo(walletName)
    } yield assert(newInfo.keypoolsize == info.keypoolsize + 1)
  }

  it should "be able to change the wallet password" in {
    val newPass = "new_password"

    for {
      walletClient <- walletClientF
      _ <- walletClient.walletLock(walletName)
      _ <-
        walletClient.walletPassphraseChange(password, newPass, Some(walletName))
      _ = password = newPass

      _ <- walletClient.walletPassphrase(password, 1000, Some(walletName))
      info <- walletClient.getWalletInfo(walletName)
      _ <- walletClient.walletLock(walletName)
      newInfo <- walletClient.getWalletInfo(walletName)
    } yield {

      assert(info.unlocked_until.nonEmpty)
      assert(info.unlocked_until.get > 0)
      assert(newInfo.unlocked_until.contains(0))
    }
  }

  it should "be able to send to an address" in {
    for {
      client <- walletClientF
      (otherClient, _, _) <- clientsF
      address <- otherClient.getNewAddress(Some(walletName))
      _ <- client.walletPassphrase(password, 1000, Some(walletName))
      txid <- client.sendToAddress(address,
                                   Bitcoins(1),
                                   walletNameOpt = Some(walletName))
      transaction <-
        client.getTransaction(txid, walletNameOpt = Some(walletName))
    } yield {
      assert(transaction.amount == Bitcoins(-1))
      assert(transaction.details.head.address.contains(address))
    }
  }

  it should "be able to send btc to many addresses" in {
    for {
      client <- walletClientF
      (otherClient, _, _) <- clientsF
      address1 <- otherClient.getNewAddress(Some(walletName))
      address2 <- otherClient.getNewAddress(Some(walletName))
      _ <- client.walletPassphrase(password, 1000, Some(walletName))
      txid <-
        client
          .sendMany(Map(address1 -> Bitcoins(1), address2 -> Bitcoins(2)),
                    walletNameOpt = Some(walletName))
      transaction <-
        client.getTransaction(txid, walletNameOpt = Some(walletName))
    } yield {
      assert(transaction.amount == Bitcoins(-3))
      assert(transaction.details.exists(_.address.contains(address1)))
      assert(transaction.details.exists(_.address.contains(address2)))
    }
  }

  it should "be able to get the balance" in {
    for {
      client <- walletClientF
      balance <- client.getBalance(walletName)
      _ <-
        client
          .getNewAddress(Some(walletName))
          .flatMap(client.generateToAddress(1, _))
      newBalance <- client.getBalance(walletName)
    } yield {
      assert(balance.toBigDecimal > 0)
      assert(balance.toBigDecimal < newBalance.toBigDecimal)
    }
  }

  it should "be able to dump a private key" in {
    for {
      client <- walletClientF
      address <- client.getNewAddress(Some(walletName))
      _ <- client.dumpPrivKey(address, Some(walletName))
    } yield succeed
  }

  it should "be able to import a private key" in {
    val ecPrivateKey = ECPrivateKey.freshPrivateKey
    val publicKey = ecPrivateKey.publicKey
    val address = P2PKHAddress(publicKey, networkParam)

    for {
      client <- walletClientF
      _ <- client.importPrivKey(ecPrivateKey,
                                rescan = false,
                                walletNameOpt = Some(walletName))
      key <- client.dumpPrivKey(address, Some(walletName))
      result <-
        client
          .dumpWallet(
            client.getDaemon.datadir.getAbsolutePath + "/wallet_dump.dat",
            Some(walletName))
    } yield {
      assert(key == ecPrivateKey)
      val reader = new Scanner(result.filename)
      var found = false
      while (reader.hasNext) {
        if (reader.next == ECPrivateKeyUtil.toWIF(ecPrivateKey, networkParam)) {
          found = true
        }
      }
      assert(found)
    }
  }

  it should "be able to import a public key" in {
    val pubKey = ECPublicKey.freshPublicKey
    for {
      client <- walletClientF
      _ <- client.importPubKey(pubKey, walletNameOpt = Some(walletName))
    } yield succeed
  }

  it should "be able to import multiple addresses with importMulti" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val address1 = P2PKHAddress(privKey.publicKey, networkParam)

    val privKey1 = ECPrivateKey.freshPrivateKey
    val privKey2 = ECPrivateKey.freshPrivateKey

    for {
      client <- walletClientF
      firstResult <-
        client
          .createMultiSig(2,
                          Vector(privKey1.publicKey, privKey2.publicKey),
                          walletNameOpt = Some(walletName))
      address2 = firstResult.address

      secondResult <-
        client
          .importMulti(
            Vector(
              RpcOpts.ImportMultiRequest(RpcOpts.ImportMultiAddress(address1),
                                         UInt32(0)),
              RpcOpts.ImportMultiRequest(RpcOpts.ImportMultiAddress(address2),
                                         UInt32(0))),
            rescan = false,
            walletNameOpt = Some(walletName)
          )
    } yield {
      assert(secondResult.length == 2)
      assert(secondResult(0).success)
      assert(secondResult(1).success)
    }
  }

  it should "be able to import a wallet" in {
    for {
      client <- walletClientF
      walletClient <- walletClientF
      address <- client.getNewAddress(Some(walletName))
      walletFile =
        client.getDaemon.datadir.getAbsolutePath + "/client_wallet.dat"

      fileResult <-
        client.dumpWallet(walletFile, walletNameOpt = Some(walletName))
      _ <- walletClient.walletPassphrase(password, 1000, Some(walletName))
      _ <- walletClient.importWallet(walletFile, Some(walletName))
      _ <- walletClient.dumpPrivKey(address, Some(walletName))
    } yield assert(fileResult.filename.exists)

  }

  it should "be able to set the tx fee" in {
    for {
      client <- walletClientF
      success <- client.setTxFee(Bitcoins(0.01), Some(walletName))
      info <- client.getWalletInfo(walletName)
    } yield {
      assert(success)
      assert(info.paytxfee == SatoshisPerByte(Satoshis(1000)))
    }
  }

  it should "be able to sign a raw transaction with the wallet" in {
    for {
      client <- walletClientF
      (otherClient, _, _) <- clientsF
      address <- otherClient.getNewAddress(Some(walletName))
      transactionWithoutFunds <-
        client
          .createRawTransaction(Vector.empty, Map(address -> Bitcoins(1)))
      transactionResult <-
        client.fundRawTransaction(transactionWithoutFunds, walletName)
      transaction = transactionResult.hex
      singedTx <-
        client
          .signRawTransactionWithWallet(transaction, Some(walletName))
          .map(_.hex)

      // Will throw error if invalid
      _ <- client.sendRawTransaction(singedTx)
    } yield {
      assert(transaction.inputs.length == 1)
      assert(
        transaction.outputs.contains(
          TransactionOutput(Bitcoins(1), address.scriptPubKey)))
    }
  }
}
