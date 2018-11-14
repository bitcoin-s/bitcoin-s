package org.bitcoins.rpc.common

import java.io.File

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.rpc.{ RpcUtil, TestUtil }
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddressType
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfterAll }

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }
import scala.async.Async.{ async, await }

class WalletRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("WalletRpcTest")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = TestUtil.network

  val client: BitcoindRpcClient = new BitcoindRpcClient(TestUtil.instance())

  // This client's wallet is encrypted
  val walletClient = new BitcoindRpcClient(TestUtil.instance())

  var password = "password"

  override def beforeAll(): Unit = {
    TestUtil.startServers(client, walletClient)
    Await.result(client.generate(200), 3.seconds)

    Await.result(
      walletClient.encryptWallet(password).map { _ =>
        RpcUtil.awaitServerShutdown(walletClient)

        // Very rarely, this may fail if bitocoind does not ping but hasn't yet released its locks
        walletClient.start()
        RpcUtil.awaitServer(walletClient)
      },
      5.seconds)
  }

  override protected def afterAll(): Unit = {
    TestUtil.stopServers(client, walletClient)
    Await.result(system.terminate(), 10.seconds)
  }

  behavior of "WalletRpc"

  it should "be able to dump the wallet" in {
    client
      .dumpWallet(client.getDaemon.authCredentials.datadir + "/test.dat")
      .map { result =>
        assert(result.filename.exists)
        assert(result.filename.isFile)
      }
  }

  it should "be able to list wallets" in {
    client.listWallets.map { wallets =>
      assert(wallets == Vector("wallet.dat"))
    }
  }

  it should "be able to backup the wallet" in {
    client
      .backupWallet(client.getDaemon.authCredentials.datadir + "/backup.dat")
      .map { _ =>
        val file =
          new File(client.getDaemon.authCredentials.datadir + "/backup.dat")
        assert(file.exists)
        assert(file.isFile)
      }
  }

  it should "be able to lock and unlock the wallet" in {
    walletClient.walletLock().flatMap { _ =>
      walletClient.walletPassphrase(password, 1000).flatMap { _ =>
        walletClient.getWalletInfo.flatMap { info =>
          assert(info.unlocked_until.nonEmpty)
          assert(info.unlocked_until.get > 0)

          walletClient.walletLock().flatMap { _ =>
            walletClient.getWalletInfo.map { newInfo =>
              assert(newInfo.unlocked_until.contains(0))
            }
          }
        }
      }
    }
  }

  it should "be able to change the wallet password - refactored" in async {
    val newPass = "new_password"

    await(walletClient.walletLock())
    await(walletClient.walletPassphraseChange(password, newPass))
    password = newPass
    await(walletClient.walletPassphrase(password, 1000))
    val info = await(walletClient.getWalletInfo)
    assert(info.unlocked_until.nonEmpty)
    assert(info.unlocked_until.get > 0)
    await(walletClient.walletLock())
    val newInfo = await(walletClient.getWalletInfo)
    assert(newInfo.unlocked_until.contains(0))
  }
}
