package org.bitcoins.rpc.common

import java.io.File
import java.util.Scanner

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{ ECPrivateKey, ECPublicKey }
import org.bitcoins.core.currency.{ Bitcoins, Satoshis }
import org.bitcoins.core.number.{ Int64, UInt32 }
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.protocol.script.{ P2SHScriptSignature, ScriptPubKey, ScriptSignature }
import org.bitcoins.core.protocol.transaction.{ TransactionInput, TransactionOutPoint }
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.rpc.client.common.RpcOpts.{ AddNodeArgument, AddressType }
import org.bitcoins.rpc.client.common.{ BitcoindRpcClient, RpcOpts }
import org.bitcoins.rpc.jsonmodels.RpcAddress
import org.bitcoins.rpc.{ BitcoindRpcTestUtil, RpcUtil }
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfter, BeforeAndAfterAll }
import org.slf4j.Logger

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }

class BitcoindRpcClientTest
  extends AsyncFlatSpec
  with BeforeAndAfterAll
  with BeforeAndAfter {
  implicit val system: ActorSystem = ActorSystem("RpcClientTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  implicit val client: BitcoindRpcClient = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())

  val otherClient = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())

  // This client's wallet is encrypted
  val walletClient = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())

  val prunedInstance = BitcoindRpcTestUtil.instance(pruneMode = true)
  val pruneClient = new BitcoindRpcClient(prunedInstance)

  val clients = Vector(client, otherClient, walletClient, pruneClient)

  val logger: Logger = BitcoinSLogger.logger

  var password = "password"

  override def beforeAll(): Unit = {
    logger.info("Temp bitcoin directory created")

    logger.info("Bitcoin servers starting")
    BitcoindRpcTestUtil.startServers(clients)

    client.addNode(otherClient.getDaemon.uri, AddNodeArgument.Add)

    Await.result(
      walletClient.encryptWallet(password).map { msg =>
        logger.info(msg)
        RpcUtil.awaitServerShutdown(walletClient)
        logger.debug(walletClient.isStarted.toString)
        // Very rarely, this may fail if bitocoind does not ping but hasn't yet released its locks
        walletClient.start()
        logger.info("Bitcoin server restarting")
        RpcUtil.awaitServer(walletClient)
      },
      5.seconds)

    logger.info("Mining some blocks")
    Await.result(client.generate(200), 3.seconds)
    Await.result(pruneClient.generate(3000), 60.seconds)

    BitcoindRpcTestUtil.awaitConnection(client, otherClient)
  }

  override def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(clients)
    Await.result(system.terminate(), 10.seconds)
  }

  behavior of "BitcoindRpcClient"

  it should "be able to prune the blockchain" in {
    pruneClient.getBlockCount.flatMap { count =>
      pruneClient.pruneBlockChain(count).flatMap { pruned =>
        assert(pruned > 0)
      }
    }
  }

  it should "be able to get the first block" in {
    BitcoindRpcTestUtil.getFirstBlock.flatMap { block =>
      assert(block.tx.nonEmpty)
      assert(block.height == 1)
    }
  }

  it should "be able to import funds without rescan and then remove them" in {
    client.getNewAddress.flatMap { address =>
      client.dumpPrivKey(address).flatMap { privKey =>
        BitcoindRpcTestUtil.fundBlockChainTransaction(client, address, Bitcoins(1.5)).flatMap {
          txid =>
            client.generate(1).flatMap { _ =>
              client.getTransaction(txid).flatMap { tx =>
                client.getTxOutProof(Vector(txid)).flatMap { proof =>
                  otherClient.getBalance.flatMap { balanceBefore =>
                    otherClient.importPrivKey(privKey, rescan = false).flatMap {
                      _ =>
                        otherClient.importPrunedFunds(tx.hex, proof).flatMap {
                          _ =>
                            otherClient.getBalance.flatMap { balanceAfter =>
                              assert(
                                balanceAfter == balanceBefore + Bitcoins(1.5))
                              otherClient.validateAddress(address).flatMap {
                                addressInfo =>
                                  assert(addressInfo.ismine.contains(true))
                                  otherClient.removePrunedFunds(txid).flatMap {
                                    _ =>
                                      otherClient.getBalance.flatMap {
                                        balance =>
                                          assert(balance == balanceBefore)
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
        }
      }
    }
  }

  it should "be able to invalidate a block" in {
    otherClient.getNewAddress.flatMap { address =>
      BitcoindRpcTestUtil.fundMemPoolTransaction(client, address, Bitcoins(3)).flatMap { txid =>
        client.getRawMemPool.flatMap { _ =>
          client.generate(1).flatMap { blocks =>
            client.invalidateBlock(blocks.head).flatMap { _ =>
              client.getRawMemPool.flatMap { mempool =>
                assert(mempool.contains(txid))
                client.getBlockCount.flatMap { count1 =>
                  otherClient.getBlockCount.flatMap { count2 =>
                    client.generate(2) // Ensure client and otherClient have the same blockchain
                    assert(count1 == count2 - 1)
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  it should "be able to submit a new block" in {
    BitcoindRpcTestUtil.createNodePair().flatMap {
      case (client1, client2) =>
        client1.disconnectNode(client2.getDaemon.uri).flatMap { _ =>
          BitcoindRpcTestUtil.awaitDisconnected(client1, client2)
          client2.generate(1).flatMap { hash =>
            client2.getBlockRaw(hash.head).flatMap { block =>
              client1.submitBlock(block).flatMap { _ =>
                client1.getBlockCount.flatMap { count =>
                  client2.getBlockCount.flatMap { count2 =>
                    assert(count == count2)
                    client1.getBlockHash(count).flatMap { hash1 =>
                      client2.getBlockHash(count).flatMap { hash2 =>
                        BitcoindRpcTestUtil.deleteNodePair(client1, client2)
                        assert(hash1 == hash2)
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

  it should "be able to mark a block as precious" in {
    BitcoindRpcTestUtil.createNodePair().flatMap {
      case (client1, client2) =>

        client1.disconnectNode(client2.getDaemon.uri).flatMap { _ =>

          BitcoindRpcTestUtil.awaitDisconnected(client1, client2)

          client1.generate(1).flatMap { blocks1 =>

            client2.generate(1).flatMap { blocks2 =>

              client1.getBestBlockHash.flatMap { bestHash1 =>
                assert(bestHash1 == blocks1.head)

                client2.getBestBlockHash.flatMap { bestHash2 =>

                  assert(bestHash2 == blocks2.head)

                  val conn = client1.addNode(client2.getDaemon.uri, AddNodeArgument.OneTry)

                  conn.flatMap { _ =>

                    //make sure the block was propogated to client2
                    RpcUtil.awaitConditionF(
                      conditionF = () => BitcoindRpcTestUtil.hasSeenBlock(client2, bestHash1))

                    val precious = client2.preciousBlock(bestHash1)

                    val client2BestBlock = precious.flatMap { _ =>
                      val b = client2.getBestBlockHash
                      b
                    }

                    client2BestBlock.map { newBestHash =>
                      BitcoindRpcTestUtil.deleteNodePair(client1, client2)
                      assert(newBestHash == bestHash1)
                    }
                  }
                }
              }
            }
          }
        }
    }
  }
  it should "be able to list address groupings" in {
    client.getNewAddress.flatMap { address =>
      BitcoindRpcTestUtil.fundBlockChainTransaction(client, address, Bitcoins(1.25)).flatMap {
        _ =>
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

  it should "be able to abandon a transaction" in {
    otherClient.getNewAddress.flatMap { address =>
      client.sendToAddress(address, Bitcoins(1)).flatMap { txid =>
        client.abandonTransaction(txid).flatMap { _ =>
          client.getTransaction(txid).map { transaction =>
            assert(transaction.details.head.abandoned.contains(true))
          }
        }
      }
    }
  }

  it should "fail to abandon a transaction which has not been sent" in {
    otherClient.getNewAddress.flatMap { address =>
      client
        .createRawTransaction(Vector(), Map(address -> Bitcoins(1)))
        .flatMap { tx =>
          recoverToSucceededIf[RuntimeException](
            client.abandonTransaction(tx.txIdBE))
        }
    }
  }

  it should "be able to list transactions by receiving addresses" in {
    otherClient.getNewAddress.flatMap { address =>
      BitcoindRpcTestUtil.fundBlockChainTransaction(client, address, Bitcoins(1.5)).flatMap {
        txid =>
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
        BitcoindRpcTestUtil.fundBlockChainTransaction(client, address, Bitcoins(1.5)).flatMap {
          txid =>
            otherClient.listReceivedByAddress(includeWatchOnly = true).map {
              list =>
                val entry =
                  list.find(addr => addr.involvesWatchonly.contains(true)).get
                assert(entry.address == address)
                assert(entry.involvesWatchonly.contains(true))
                assert(entry.amount == Bitcoins(1.5))
                assert(entry.txids.head == txid)
            }
        }
      }
    }
  }

  it should "be able to get a transaction" in {
    BitcoindRpcTestUtil.getFirstBlock.flatMap { block =>
      client.getTransaction(block.tx.head.txid).flatMap { tx =>
        assert(tx.txid == block.tx.head.txid)
        assert(tx.amount == Bitcoins(50))
        assert(tx.blockindex.get == 0)
        assert(tx.details.head.category == "generate")
        assert(tx.generated.get)
        client.getBlockCount.map { count =>
          assert(tx.confirmations == count)
        }
      }
    }
  }

  it should "be able to get an address from bitcoind" in {
    client.getNewAddress.map { _ =>
      succeed
    }
  }

  it should "be able to get a new raw change address" in {
    client.getRawChangeAddress.map { _ =>
      succeed
    }

    client.getRawChangeAddress(AddressType.Legacy).map { _ =>
      succeed
    }

    client.getRawChangeAddress(AddressType.P2SHSegwit).map { _ =>
      succeed
    }

    client.getRawChangeAddress(AddressType.Bech32).map { _ =>
      succeed
    }
  }

  it should "be able to get the amount recieved by some address" in {
    client.getNewAddress.flatMap { address =>
      client.getReceivedByAddress(address).flatMap { amount =>
        assert(amount == Bitcoins(0))
      }
    }
  }

  it should "be able to get the tx outset info" in {
    client.getTxOutSetInfo.flatMap { info =>
      client.getBlockCount.map { count =>
        assert(info.height == count)
      }
      client.getBestBlockHash.map { hash =>
        assert(info.bestblock == hash)
      }
    }
  }

  it should "be able to get the unconfirmed balance" in { // otherClient isn't receiving txs from client???
    client.getUnconfirmedBalance.flatMap { balance =>
      assert(balance == Bitcoins(0))
      BitcoindRpcTestUtil.sendCoinbaseTransaction(client, client).flatMap { transaction =>
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

  it should "be able to get the block count" in {
    client.getBlockCount.flatMap { count =>
      assert(count >= 0)
      otherClient.getBlockCount.map { otherCount =>
        assert(count == otherCount)
      }
    }
  }

  it should "be able to get the connection count" in {
    client.getConnectionCount.map { count =>
      assert(count == 1)
    }
  }

  it should "be able to get the best block hash" in {
    client.getBestBlockHash.map { _ =>
      succeed
    }
  }

  it should "be able to get the chain tips" in {
    client.getChainTips.map { _ =>
      succeed
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

  it should "be able to get block hash by height" in {
    client.generate(2).flatMap { blocks =>
      client.getBlockCount.flatMap { count =>
        client.getBlockHash(count).flatMap { hash =>
          assert(blocks(1) == hash)
          client.getBlockHash(count - 1).map { hash =>
            assert(blocks(0) == hash)
          }
        }
      }
    }
  }

  it should "be able to get help from bitcoind" in {
    client.help().flatMap { genHelp =>
      assert(!genHelp.isEmpty)
      client.help("help").map { helpHelp =>
        assert(genHelp != helpHelp)
        assert(!helpHelp.isEmpty)
      }
    }
  }

  it should "be able to get a raw block" in {
    client.generate(1).flatMap { blocks =>
      client.getBlockRaw(blocks(0)).flatMap { block =>
        client.getBlockHeaderRaw(blocks(0)).map { blockHeader =>
          assert(block.blockHeader == blockHeader)
        }
      }
    }
  }

  it should "be able to get a block" in {
    client.generate(1).flatMap { blocks =>
      client.getBlock(blocks(0)).flatMap { block =>
        assert(block.hash == blocks(0))
        assert(block.confirmations == 1)
        assert(block.size > 0)
        assert(block.weight > 0)
        assert(block.height > 0)
        assert(block.difficulty > 0)
      }
    }
  }

  it should "be able to get a block with verbose transactions" in {
    client.generate(2).flatMap { blocks =>
      client.getBlockWithTransactions(blocks(1)).flatMap { block =>
        assert(block.hash == blocks(1))
        assert(block.tx.length == 1)
        val tx = block.tx.head
        assert(tx.vout.head.n == 0)
      }
    }
  }

  it should "be able to list all blocks since a given block" in {
    client.generate(3).flatMap { blocks =>
      client.listSinceBlock(blocks(0)).map { list =>
        assert(list.transactions.length >= 2)
        assert(list.transactions.exists(_.blockhash.contains(blocks(1))))
        assert(list.transactions.exists(_.blockhash.contains(blocks(2))))
      }
    }
  }

  it should "be able to list transactions in a given range" in { // Assumes 30 transactions
    client.listTransactions().flatMap { list1 =>
      client.listTransactions(count = 20).flatMap { list2 =>
        assert(list2.takeRight(10) == list1)
        client.listTransactions(count = 20, skip = 10).map { list3 =>
          assert(list2.splitAt(10)._1 == list3.takeRight(10))
        }
      }
    }
  }

  it should "be able to validate a bitcoin address" in {
    otherClient.getNewAddress.flatMap { address =>
      client.validateAddress(address).map { validation =>
        assert(validation.isvalid)
      }
    }
  }

  it should "be able to verify the chain" in {
    client.verifyChain(blocks = 0).map { valid =>
      assert(valid)
    }
  }

  it should "be able to decode a reedem script" in {
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val pubKey1 = ecPrivKey1.publicKey

    client.getNewAddress(addressType = AddressType.Legacy).flatMap { address =>
      client
        .addMultiSigAddress(
          2,
          Vector(
            Left(pubKey1),
            Right(address.asInstanceOf[P2PKHAddress])))
        .flatMap { multisig =>
          client.decodeScript(multisig.redeemScript).map { decoded =>
            assert(decoded.reqSigs.contains(2))
            assert(decoded.typeOfScript.contains("multisig"))
            assert(decoded.addresses.get.contains(address))
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
              if (reader.next() == ecPrivateKey.toWIF(networkParam)) {
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
              RpcOpts.ImportMultiRequest(
                RpcOpts.ImportMultiAddress(address1),
                UInt32(0)),
              RpcOpts.ImportMultiRequest(
                RpcOpts.ImportMultiAddress(address2),
                UInt32(0))),
            rescan = false)
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

  it should "be able to get the chain tx stats" in {
    client.getChainTxStats.map { stats =>
      assert(stats.time > UInt32(0))
      assert(stats.txcount > 0)
      assert(stats.window_block_count > 0)
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

  it should "be able to save the mem pool to disk" in {
    val regTest =
      new File(client.getDaemon.authCredentials.datadir + "/regtest")
    assert(regTest.isDirectory)
    assert(!regTest.list().contains("mempool.dat"))
    client.saveMemPool().map { _ =>
      assert(regTest.list().contains("mempool.dat"))
    }
  }

  it should "be able to get and set the logging configuration" in {
    client.logging.flatMap { info =>
      info.keySet.foreach(category => assert(info(category) == 1))
      client.logging(exclude = Vector("qt")).map { info =>
        assert(info("qt") == 0)
      }
    }
  }

  it should "be able to get the client's uptime" in {
    client.uptime.flatMap { time1 =>
      assert(time1 > UInt32(0))
      otherClient.uptime.map { time2 =>
        assert(time1 >= time2)
      }
    }
  }

  it should "be able to sign a raw transaction" in {
    client.getNewAddress.flatMap { address =>
      client.validateAddress(address).flatMap { addressInfo =>
        client
          .addMultiSigAddress(1, Vector(Left(addressInfo.pubkey.get)))
          .flatMap { multisig =>
            BitcoindRpcTestUtil.fundBlockChainTransaction(client, multisig.address, Bitcoins(1.2))
              .flatMap { txid =>
                client.getTransaction(txid).flatMap { rawTx =>
                  client.decodeRawTransaction(rawTx.hex).flatMap { tx =>
                    val output =
                      tx.vout.find(output => output.value == Bitcoins(1.2)).get
                    val input = TransactionInput(
                      TransactionOutPoint(txid.flip, UInt32(output.n)),
                      P2SHScriptSignature(multisig.redeemScript.hex),
                      UInt32.max - UInt32.one)
                    client.getNewAddress.flatMap { newAddress =>
                      client
                        .createRawTransaction(
                          Vector(input),
                          Map(newAddress -> Bitcoins(1.1)))
                        .flatMap { ctx =>
                          client
                            .signRawTransaction(
                              ctx,
                              Vector(
                                RpcOpts.SignRawTransactionOutputParameter(
                                  txid,
                                  output.n,
                                  ScriptPubKey.fromAsmHex(
                                    output.scriptPubKey.hex),
                                  Some(multisig.redeemScript),
                                  Bitcoins(1.2))))
                            .map { result =>
                              assert(result.complete)
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

  it should "be able to combine raw transacitons" in {
    client.getNewAddress.flatMap { address1 =>
      otherClient.getNewAddress.flatMap { address2 =>
        client.validateAddress(address1).flatMap { address1Info =>
          otherClient.validateAddress(address2).flatMap { address2Info =>
            client
              .addMultiSigAddress(
                2,
                Vector(
                  Left(address1Info.pubkey.get),
                  Left(address2Info.pubkey.get)))
              .flatMap { multisig =>
                otherClient
                  .addMultiSigAddress(
                    2,
                    Vector(
                      Left(address1Info.pubkey.get),
                      Left(address2Info.pubkey.get)))
                  .flatMap { _ =>
                    client.validateAddress(multisig.address).flatMap {
                      _ =>
                        BitcoindRpcTestUtil.fundBlockChainTransaction(
                          client,
                          multisig.address,
                          Bitcoins(1.2)).flatMap {
                            txid =>
                              client.getTransaction(txid).flatMap { rawTx =>
                                client.decodeRawTransaction(rawTx.hex).flatMap {
                                  tx =>
                                    val output = tx.vout
                                      .find(output =>
                                        output.value == Bitcoins(1.2))
                                      .get
                                    val input = TransactionInput(
                                      TransactionOutPoint(
                                        txid.flip,
                                        UInt32(output.n)),
                                      P2SHScriptSignature(
                                        multisig.redeemScript.hex),
                                      UInt32.max - UInt32.one)
                                    client.getNewAddress.flatMap { address =>
                                      otherClient
                                        .createRawTransaction(
                                          Vector(input),
                                          Map(address -> Bitcoins(1.1)))
                                        .flatMap { ctx =>
                                          client
                                            .signRawTransaction(
                                              ctx,
                                              Vector(RpcOpts
                                                .SignRawTransactionOutputParameter(
                                                  txid,
                                                  output.n,
                                                  ScriptPubKey.fromAsmHex(
                                                    output.scriptPubKey.hex),
                                                  Some(multisig.redeemScript),
                                                  Bitcoins(1.2))))
                                            .flatMap { partialTx1 =>
                                              assert(!partialTx1.complete)
                                              assert(partialTx1.hex != ctx)
                                              otherClient
                                                .signRawTransaction(
                                                  ctx,
                                                  Vector(
                                                    RpcOpts.SignRawTransactionOutputParameter(
                                                      txid,
                                                      output.n,
                                                      ScriptPubKey.fromAsmHex(
                                                        output.scriptPubKey.hex),
                                                      Some(multisig.redeemScript),
                                                      Bitcoins(1.2))))
                                                .flatMap { partialTx2 =>
                                                  assert(!partialTx2.complete)
                                                  assert(partialTx2.hex != ctx)
                                                  client
                                                    .combineRawTransaction(
                                                      Vector(
                                                        partialTx1.hex,
                                                        partialTx2.hex))
                                                    .flatMap { combinedTx =>
                                                      client
                                                        .sendRawTransaction(
                                                          combinedTx)
                                                        .map { _ =>
                                                          succeed
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
                  }
              }
          }
        }
      }
    }
  }

  it should "be able to bump a mem pool tx fee" in {
    otherClient.getNewAddress.flatMap { address =>
      client.listUnspent.flatMap { unspent =>
        val output = unspent.find(output => output.amount.toBigDecimal > 1).get
        val input: TransactionInput = TransactionInput(
          TransactionOutPoint(output.txid.flip, UInt32(output.vout)),
          ScriptSignature.empty,
          UInt32.max - UInt32(2))
        client.getRawChangeAddress.flatMap { changeAddress =>
          client
            .createRawTransaction(
              Vector(input),
              Map(
                address -> Bitcoins(0.5),
                changeAddress -> Bitcoins(output.amount.toBigDecimal - 0.55)))
            .flatMap { ctx =>
              client.signRawTransaction(ctx).flatMap { stx =>
                client.sendRawTransaction(stx.hex, allowHighFees = true).flatMap { txid =>
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

  it should "be able to rescan the blockchain" in {
    client.rescanBlockChain().flatMap { result =>
      assert(result.start_height == 0)
      client.getBlockCount.map { count =>
        assert(count == result.stop_height)
      }
    }
  }

  it should "be able to add and remove a node" in {
    otherClient.addNode(walletClient.getDaemon.uri, AddNodeArgument.Add).flatMap { _ =>
      BitcoindRpcTestUtil.awaitConnection(otherClient, walletClient)
      otherClient.getAddedNodeInfo(walletClient.getDaemon.uri).flatMap { info =>
        assert(info.length == 1)
        assert(info.head.addednode == walletClient.getDaemon.uri)
        assert(info.head.connected.contains(true))

        otherClient.addNode(walletClient.getDaemon.uri, AddNodeArgument.Remove).flatMap { _ =>
          otherClient.getAddedNodeInfo.map { newInfo =>
            assert(newInfo.isEmpty)
          }
        }
      }
    }
  }

  it should "be able to add and disconnect a node" in {
    otherClient.addNode(walletClient.getDaemon.uri, AddNodeArgument.Add).flatMap { _ =>
      BitcoindRpcTestUtil.awaitConnection(otherClient, walletClient)
      otherClient.getAddedNodeInfo(walletClient.getDaemon.uri).flatMap { info =>
        assert(info.head.connected.contains(true))

        otherClient.disconnectNode(walletClient.getDaemon.uri).flatMap { _ =>
          BitcoindRpcTestUtil.awaitDisconnected(otherClient, walletClient)
          otherClient.getAddedNodeInfo(walletClient.getDaemon.uri).map {
            newInfo =>
              assert(newInfo.head.connected.contains(false))
          }
        }
      }
    }
  }
}
