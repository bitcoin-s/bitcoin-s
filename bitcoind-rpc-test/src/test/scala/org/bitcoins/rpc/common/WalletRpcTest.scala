package org.bitcoins.rpc.common

import java.io.File
import java.util.Scanner

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{ECPrivateKey, ECPublicKey}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.rpc.{BitcoindRpcTestConfig, BitcoindRpcTestUtil}
import org.bitcoins.rpc.client.common.RpcOpts.{AddNodeArgument, AddressType}
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, RpcOpts}
import org.bitcoins.rpc.jsonmodels.RpcAddress
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.async.Async.{async, await}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Properties

class WalletRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("WalletRpcTest")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  private implicit val client: BitcoindRpcClient = new BitcoindRpcClient(
    BitcoindRpcTestUtil.instance())
  private val otherClient = new BitcoindRpcClient(
    BitcoindRpcTestUtil.instance())

  private val logger = BitcoinSLogger.logger

  // sending a TX from client -> client causes the balance calculation to
  // sometimes end up as 0, making the result of otherClient importing
  // the privkey effectively null. instead thirdClient generates an address
  // and dumps the key, client sends the TX and otherClient imports the
  // key
  private val thirdClient = new BitcoindRpcClient(
    BitcoindRpcTestUtil.instance())

  // This client's wallet is encrypted
  val walletClient = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())

  var password = "password"

  override def beforeAll(): Unit = {
    import BitcoindRpcTestConfig.DEFAULT_TIMEOUT
    val startF = BitcoindRpcTestUtil.startServers(
      Vector(client, walletClient, otherClient, thirdClient))
    Await.result(startF, DEFAULT_TIMEOUT)

    val addNodeF = client.addNode(otherClient.instance.uri, AddNodeArgument.Add)
    Await.result(addNodeF, DEFAULT_TIMEOUT)

    BitcoindRpcTestUtil.awaitConnection(client, otherClient)

    Await.result(client.generate(200), DEFAULT_TIMEOUT)

    Await.result(walletClient.encryptWallet(password), DEFAULT_TIMEOUT)
    BitcoindRpcTestUtil.awaitServerShutdown(walletClient)

    // Very rarely, this may fail if bitcoind does not ping but hasn't yet released its locks
    // fails more often on mac
    if (Properties.isMac) Thread.sleep(1000)
    Await.result(walletClient.start(), DEFAULT_TIMEOUT)
  }

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(
      Vector(client, walletClient, otherClient, thirdClient))
    TestKit.shutdownActorSystem(system)
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
    for {
      _ <- walletClient.walletLock()
      _ <- walletClient.walletPassphrase(password, 1000)
      info <- walletClient.getWalletInfo
      _ = {
        assert(info.unlocked_until.nonEmpty)
        assert(info.unlocked_until.get > 0)
      }
      _ <- walletClient.walletLock()

      newInfo <- walletClient.getWalletInfo
    } yield assert(newInfo.unlocked_until.contains(0))
  }

  it should "be able to get an address from bitcoind" in {
    Future
      .sequence(
        List(client.getNewAddress,
             client.getNewAddress(AddressType.Bech32),
             client.getNewAddress(AddressType.P2SHSegwit),
             client.getNewAddress(AddressType.Legacy)))
      .flatMap(_ => succeed)
  }

  it should "be able to get a new raw change address" in {
    Future
      .sequence(
        List(
          client.getRawChangeAddress,
          client.getRawChangeAddress(AddressType.Legacy),
          client.getRawChangeAddress(AddressType.Bech32),
          client.getRawChangeAddress(AddressType.P2SHSegwit)
        )
      )
      .map(_ => succeed)
  }

  it should "be able to get the amount recieved by some address" in {
    client.getNewAddress.flatMap { address =>
      client.getReceivedByAddress(address).flatMap { amount =>
        assert(amount == Bitcoins(0))
      }
    }
  }

  it should "be able to get the unconfirmed balance" in { // otherClient isn't receiving txs from client???
    client.getUnconfirmedBalance.flatMap { balance =>
      assert(balance == Bitcoins(0))
      BitcoindRpcTestUtil.sendCoinbaseTransaction(client, client).flatMap {
        transaction =>
          client.getUnconfirmedBalance.map { newBalance =>
            assert(newBalance == transaction.amount)
          }
      }
    }
  }

  it should "be able to get the wallet info" in {
    client.getWalletInfo.map { info =>
      assert(info.balance.toBigDecimal > 0)
      assert(info.txcount > 0)
      assert(info.keypoolsize > 0)
      assert(!info.unlocked_until.contains(0))
    }
  }

  it should "be able to refill the keypool" in {
    client.getWalletInfo.flatMap { info =>
      client.keyPoolRefill(info.keypoolsize + 1).flatMap { _ =>
        client.getWalletInfo.flatMap { newInfo =>
          assert(newInfo.keypoolsize == info.keypoolsize + 1)
        }
      }
    }
  }

  it should "be able to change the wallet password" in async {
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

  it should "be able to import funds without rescan and then remove them" in async {

    val address = await(thirdClient.getNewAddress)
    val privKey = await(thirdClient.dumpPrivKey(address))

    val txidF =
      BitcoindRpcTestUtil
        .fundBlockChainTransaction(client, address, Bitcoins(1.5))
    val txid = await(txidF)

    await(client.generate(1))

    val tx = await(client.getTransaction(txid))

    val proof = await(client.getTxOutProof(Vector(txid)))

    val balanceBefore = await(otherClient.getBalance)

    await(otherClient.importPrivKey(privKey, rescan = false))
    await(otherClient.importPrunedFunds(tx.hex, proof))

    val balanceAfter = await(otherClient.getBalance)
    assert(balanceAfter == balanceBefore + Bitcoins(1.5))

    val addressInfo = await(otherClient.validateAddress(address))
    assert(addressInfo.ismine.contains(true))

    await(otherClient.removePrunedFunds(txid))

    val balance = await(otherClient.getBalance)
    assert(balance == balanceBefore)
  }

  it should "be able to list address groupings" in {
    client.getNewAddress.flatMap { address =>
      BitcoindRpcTestUtil
        .fundBlockChainTransaction(client, address, Bitcoins(1.25))
        .flatMap { _ =>
          client.listAddressGroupings.flatMap { groupings =>
            val rpcAddress =
              groupings.find(vec => vec.head.address == address).get.head
            assert(rpcAddress.address == address)
            assert(rpcAddress.balance == Bitcoins(1.25))

            BitcoindRpcTestUtil.getFirstBlock.flatMap { block =>
              val firstAddress =
                block.tx.head.vout.head.scriptPubKey.addresses.get.head
              assert(
                groupings
                  .max(Ordering.by[Vector[RpcAddress], BigDecimal](addr =>
                    addr.head.balance.toBigDecimal))
                  .head
                  .address == firstAddress)
            }
          }
        }
    }
  }

  it should "be able to send to an address" in {
    otherClient.getNewAddress.flatMap { address =>
      client.sendToAddress(address, Bitcoins(1)).flatMap { txid =>
        client.getTransaction(txid).map { transaction =>
          assert(transaction.amount == Bitcoins(-1))
          assert(transaction.details.head.address.contains(address))
        }
      }
    }
  }

  it should "be able to send btc to many addresses" in {
    otherClient.getNewAddress.flatMap { address1 =>
      otherClient.getNewAddress.flatMap { address2 =>
        client
          .sendMany(Map(address1 -> Bitcoins(1), address2 -> Bitcoins(2)))
          .flatMap { txid =>
            client.getTransaction(txid).map { transaction =>
              assert(transaction.amount == Bitcoins(-3))
              assert(transaction.details(0).address.contains(address1))
              assert(transaction.details(1).address.contains(address2))
            }
          }
      }
    }
  }

  it should "be able to list transactions by receiving addresses" in {
    otherClient.getNewAddress.flatMap { address =>
      BitcoindRpcTestUtil
        .fundBlockChainTransaction(client, address, Bitcoins(1.5))
        .flatMap { txid =>
          otherClient.listReceivedByAddress().map { receivedList =>
            val entryList =
              receivedList.filter(entry => entry.address == address)
            assert(entryList.length == 1)
            val entry = entryList.head
            assert(entry.txids.head == txid)
            assert(entry.address == address)
            assert(entry.amount == Bitcoins(1.5))
            assert(entry.confirmations == 1)
          }
        }
    }
  }

  it should "be able to import an address" in {
    client.getNewAddress.flatMap { address =>
      otherClient.importAddress(address).flatMap { _ =>
        BitcoindRpcTestUtil
          .fundBlockChainTransaction(client, address, Bitcoins(1.5))
          .flatMap { txid =>
            otherClient.listReceivedByAddress(includeWatchOnly = true).map {
              list =>
                val entry =
                  list
                    .find(addr => addr.involvesWatchonly.contains(true))
                    .get
                assert(entry.address == address)
                assert(entry.involvesWatchonly.contains(true))
                assert(entry.amount == Bitcoins(1.5))
                assert(entry.txids.head == txid)
            }
          }
      }
    }
  }

  it should "be able to get the balance" in {
    client.getBalance.flatMap { balance =>
      assert(balance.toBigDecimal > 0)
      client.generate(1).flatMap { _ =>
        client.getBalance.map { newBalance =>
          assert(balance.toBigDecimal < newBalance.toBigDecimal)
        }
      }
    }
  }

  it should "be able to dump a private key" in {
    client.getNewAddress.flatMap { address =>
      client.dumpPrivKey(address).map { _ =>
        succeed
      }
    }
  }

  it should "be able to import a private key" in {
    val ecPrivateKey = ECPrivateKey.freshPrivateKey
    val publicKey = ecPrivateKey.publicKey
    val address = P2PKHAddress(publicKey, networkParam)

    client.importPrivKey(ecPrivateKey, rescan = false).flatMap { _ =>
      client.dumpPrivKey(address).flatMap { key =>
        assert(key == ecPrivateKey)
        client
          .dumpWallet(
            client.getDaemon.authCredentials.datadir + "/wallet_dump.dat")
          .flatMap { result =>
            val reader = new Scanner(result.filename)
            var found = false
            while (reader.hasNext) {
              if (reader.next == ecPrivateKey.toWIF(networkParam)) {
                found = true
              }
            }
            assert(found)
          }
      }
    }
  }

  it should "be able to import a public key" in {
    val pubKey = ECPublicKey.freshPublicKey

    client.importPubKey(pubKey).map { _ =>
      succeed
    }
  }

  it should "be able to import multiple addresses with importMulti" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val address1 = P2PKHAddress(privKey.publicKey, networkParam)

    val privKey1 = ECPrivateKey.freshPrivateKey
    val privKey2 = ECPrivateKey.freshPrivateKey

    client
      .createMultiSig(2, Vector(privKey1.publicKey, privKey2.publicKey))
      .flatMap { result =>
        val address2 = result.address

        client
          .importMulti(
            Vector(
              RpcOpts.ImportMultiRequest(RpcOpts.ImportMultiAddress(address1),
                                         UInt32(0)),
              RpcOpts.ImportMultiRequest(RpcOpts.ImportMultiAddress(address2),
                                         UInt32(0))),
            rescan = false
          )
          .flatMap { result =>
            assert(result.length == 2)
            assert(result(0).success)
            assert(result(1).success)
          }
      }
  }

  it should "be able to import a wallet" in {
    client.getNewAddress.flatMap { address =>
      client
        .dumpWallet(
          client.getDaemon.authCredentials.datadir + "/client_wallet.dat")
        .flatMap { fileResult =>
          assert(fileResult.filename.exists)
          walletClient.walletPassphrase(password, 1000).flatMap { _ =>
            walletClient
              .importWallet(
                client.getDaemon.authCredentials.datadir + "/client_wallet.dat")
              .flatMap { _ =>
                walletClient
                  .dumpPrivKey(address)
                  .flatMap { _ =>
                    succeed
                  }
              }
          }
        }
    }
  }

  it should "be able to set the tx fee" in {
    client.setTxFee(Bitcoins(0.01)).flatMap { success =>
      assert(success)
      client.getWalletInfo.map { info =>
        assert(info.paytxfee == SatoshisPerByte(Satoshis(Int64(1000))))
      }
    }
  }

  it should "be able to bump a mem pool tx fee" in {
    otherClient.getNewAddress.flatMap { address =>
      client.listUnspent.flatMap { unspent =>
        val output =
          unspent.find(output => output.amount.toBigDecimal > 1).get
        val input =
          TransactionInput(TransactionOutPoint(output.txid.flip,
                                               UInt32(output.vout)),
                           ScriptSignature.empty,
                           UInt32.max - UInt32(2))
        client.getRawChangeAddress.flatMap { changeAddress =>
          client
            .createRawTransaction(
              Vector(input),
              Map(address -> Bitcoins(0.5),
                  changeAddress -> Bitcoins(output.amount.toBigDecimal - 0.55)))
            .flatMap {
              BitcoindRpcTestUtil
                .signRawTransaction(client, _)
                .flatMap { stx =>
                  client
                    .sendRawTransaction(stx.hex, allowHighFees = true)
                    .flatMap { txid =>
                      client.getTransaction(txid).flatMap { tx =>
                        client.bumpFee(txid).flatMap { bumpedTx =>
                          assert(tx.fee.get < bumpedTx.fee)
                        }
                      }
                    }
                }
            }
        }
      }
    }
  }

}
