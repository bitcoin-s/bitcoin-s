package org.bitcoins.rpc

import java.io.File
import java.net.URI
import java.util.Scanner

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey, ECPublicKey}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}
import org.bitcoins.rpc.client.{RpcClient, RpcOpts}
import org.scalatest.{AsyncFlatSpec, BeforeAndAfter, BeforeAndAfterAll}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.{BitcoinAddress, P2PKHAddress, P2SHAddress}
import org.bitcoins.core.protocol.script.{
  P2SHScriptPubKey,
  P2SHScriptSignature,
  ScriptPubKey,
  ScriptSignature
}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.rpc.client.RpcOpts.AddressType
import org.bitcoins.rpc.jsonmodels.{
  GetBlockWithTransactionsResult,
  GetTransactionResult,
  RpcAddress
}
import play.api.libs.json.Json

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

class RpcClientTest
    extends AsyncFlatSpec
    with BeforeAndAfterAll
    with BeforeAndAfter {
  implicit val system = ActorSystem()
  implicit val m = ActorMaterializer()
  implicit val ec = m.executionContext
  implicit val networkParam = TestUtil.network

  val client = new RpcClient(
    TestUtil.instance(networkParam.port + 5, networkParam.rpcPort + 5))

  val otherClient = new RpcClient(
    TestUtil.instance(networkParam.port + 10, networkParam.rpcPort + 10))

  val walletClient = new RpcClient(
    TestUtil.instance(networkParam.port + 20, networkParam.rpcPort + 20))

  val pruneClient = new RpcClient(
    TestUtil.instance(networkParam.port + 15, networkParam.rpcPort + 15, true))

  val logger = BitcoinSLogger.logger

  var password = "password"

  P2SHScriptSignature(
    "5221031ef932a3e39f7273534fd51a387b2d3bb2a663cd92739840167da6b5dd07b90e21023ba657cfddf8ce4a78df941d42ddb394c61234cd4b66ce45de223382752194d352ae")

  val redeemScript = ScriptPubKey.fromAsmHex(
    "5221031ef932a3e39f7273534fd51a387b2d3bb2a663cd92739840167da6b5dd07b90e21023ba657cfddf8ce4a78df941d42ddb394c61234cd4b66ce45de223382752194d352ae")
  BitcoinSUtil.encodeHex(P2SHScriptPubKey(redeemScript).asmBytes)

  private def createRawCoinbaseTransaction(
      sender: RpcClient = client,
      receiver: RpcClient = otherClient,
      amount: Bitcoins = Bitcoins(1)): Future[Transaction] = {
    sender.generate(2).flatMap { blocks =>
      sender.getBlock(blocks(0)).flatMap { block0 =>
        sender.getBlock(blocks(1)).flatMap { block1 =>
          sender.getTransaction(block0.tx(0)).flatMap { transaction0 =>
            sender.getTransaction(block1.tx(0)).flatMap { transaction1 =>
              val input0 =
                TransactionOutPoint(transaction0.txid,
                                    UInt32(transaction0.blockindex.get))
              val input1 =
                TransactionOutPoint(transaction1.txid,
                                    UInt32(transaction1.blockindex.get))
              val sig: ScriptSignature = ScriptSignature.empty
              receiver.getNewAddress().flatMap { address =>
                sender.createRawTransaction(
                  Vector(TransactionInput(input0, sig, UInt32(1)),
                         TransactionInput(input1, sig, UInt32(2))),
                  Map(address -> amount))
              }
            }
          }
        }
      }
    }
  }

  private def sendCoinbaseTransaction(
      sender: RpcClient = client,
      receiver: RpcClient = otherClient,
      amount: Bitcoins = Bitcoins(1)): Future[GetTransactionResult] = {
    createRawCoinbaseTransaction(sender, receiver, amount).flatMap {
      transaction =>
        sender.signRawTransaction(transaction).flatMap { signedTransaction =>
          sender
            .generate(100)
            .flatMap { _ => // Can't spend coinbase until depth 100
              sender.sendRawTransaction(signedTransaction.hex, true).flatMap {
                transactionHash =>
                  sender.getTransaction(transactionHash)
              }
            }
        }
    }
  }

  private def fundMemPoolTransaction(
      sender: RpcClient,
      address: BitcoinAddress,
      amount: Bitcoins): Future[DoubleSha256Digest] = {
    sender.createRawTransaction(Vector.empty, Map(address -> amount)).flatMap {
      createdTx =>
        sender.fundRawTransaction(createdTx).flatMap { fundedTx =>
          sender.signRawTransaction(fundedTx.hex).flatMap { signedTx =>
            sender.sendRawTransaction(signedTx.hex)
          }
        }
    }
  }

  private def fundBlockChainTransaction(
      sender: RpcClient,
      address: BitcoinAddress,
      amount: Bitcoins): Future[DoubleSha256Digest] = {
    fundMemPoolTransaction(sender, address, amount).flatMap { txid =>
      sender.generate(1).map { _ =>
        txid
      }
    }
  }

  private def getFirstBlock(
      node: RpcClient = client): Future[GetBlockWithTransactionsResult] = {
    node.getBlockHash(1).flatMap { hash =>
      node.getBlockWithTransactions(hash)
    }
  }

  override def beforeAll(): Unit = {
    println("Temp bitcoin directory created")
    println("Temp bitcoin directory created")
    println("Temp bitcoin directory created")

    walletClient.start()
    println("Bitcoin server starting")
    Thread.sleep(3000)
    client.start()
    println("Bitcoin server starting")
    Thread.sleep(3000)
    otherClient.start()
    println("Bitcoin server starting")
    Thread.sleep(3000)
    pruneClient.start()
    println("Bitcoin server starting")
    Thread.sleep(3000)

    client.addNode(otherClient.getDaemon.uri, "add")

    Await.result(
      walletClient.encryptWallet(password).map { msg =>
        println(msg)
        Thread.sleep(3000)
        walletClient.start()
        println("Bitcoin server restarting")
        Thread.sleep(4000)
      },
      15.seconds
    )

    // Mine some blocks
    println("Mining some blocks")
    Await.result(client.generate(200), 3.seconds)
    Await.result(pruneClient.generate(3000), 60.seconds)
  }

  behavior of "RpcClient"

  it should "be able to prune the blockchain" in {
    pruneClient.getBlockCount.flatMap { count =>
      pruneClient.pruneBlockChain(count).flatMap { pruned =>
        assert(pruned > 0)
      }
    }
  }

  it should "be able to import funds without rescan and then remove them" in {
    client.getNewAddress().flatMap { address =>
      client.dumpPrivKey(address).flatMap { privKey =>
        fundBlockChainTransaction(client, address, Bitcoins(1.5)).flatMap {
          txid =>
            client.generate(1).flatMap { _ =>
              client.getTransaction(txid).flatMap { tx =>
                client.getTxOutProof(Vector(txid)).flatMap { proof =>
                  otherClient.getBalance.flatMap { balanceBefore =>
                    otherClient.importPrivKey(privKey, rescan = false).flatMap {
                      _ =>
                        otherClient.importPrunedFunds(tx.hex, proof).flatMap {
                          _ =>
                            otherClient.getBalance.map { balanceAfter =>
                              assert(
                                balanceAfter == balanceBefore + Bitcoins(1.5))
                            }
                            otherClient.validateAddress(address).map {
                              addressInfo =>
                                assert(addressInfo.ismine.contains(true))
                            }
                            otherClient.removePrunedFunds(txid).flatMap { _ =>
                              otherClient.getBalance.flatMap { balance =>
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

  it should "be able to invalidate a block" in {
    otherClient.getNewAddress().flatMap { address =>
      fundMemPoolTransaction(client, address, Bitcoins(3)).flatMap { txid =>
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

  it should "be able to get peer info" in {
    client.getPeerInfo.flatMap { infoList =>
      assert(infoList.length == 1)
      val info = infoList.head
      assert(!info.inbound)
      assert(info.addnode)
      assert(info.networkInfo.addr == otherClient.getDaemon.uri)
    }
  }

  it should "be able to get the added node info" in {
    client.getAddedNodeInfo.flatMap { info =>
      assert(info.length == 1)
      assert(info.head.addednode == otherClient.getDaemon.uri)
      assert(info.head.connected.contains(true))
    }
  }

  it should "be able to ban and clear the ban of a subnet" in {
    val loopBack = URI.create("http://127.0.0.1")
    client.setBan(loopBack, "add").flatMap { _ =>
      client.listBanned.flatMap { list =>
        assert(list.length == 1)
        assert(list.head.address.getAuthority == loopBack.getAuthority)
        assert(list.head.banned_until - list.head.ban_created == UInt32(86400))

        client.setBan(loopBack, "remove").flatMap { _ =>
          client.listBanned.flatMap { newList =>
            assert(newList.isEmpty)
            client.addNode(otherClient.getDaemon.uri, "onetry").map { _ =>
              succeed
            }
          }
        }
      }
    }
  }

  it should "be able to clear banned subnets" in {
    client.setBan(URI.create("http://127.0.0.1"), "add").flatMap { _ =>
      client.setBan(URI.create("http://127.0.0.2"), "add").flatMap { _ =>
        client.listBanned.flatMap { list =>
          assert(list.length == 2)

          client.clearBanned().flatMap { _ =>
            client.listBanned.flatMap { newList =>
              assert(newList.isEmpty)
              client.addNode(otherClient.getDaemon.uri, "onetry").map { _ =>
                succeed
              }
            }
          }
        }
      }
    }
  }

  it should "be able to submit a new block" in {
    client.disconnectNode(otherClient.getDaemon.uri).flatMap { _ =>
      otherClient.generate(1).flatMap { hash =>
        otherClient.getBlockRaw(hash.head).flatMap { block =>
          client.submitBlock(block).flatMap { _ =>
            client.getBlockCount.flatMap { count =>
              client.getBlockHash(count).flatMap { hash1 =>
                otherClient.getBlockHash(count).flatMap { hash2 =>
                  assert(hash1 == hash2)
                  client.addNode(otherClient.getDaemon.uri, "onetry").map { _ =>
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

  it should "be able to mark a block as precious" in {
    client.disconnectNode(otherClient.getDaemon.uri).flatMap { _ =>
      client.generate(1).flatMap { blocks1 =>
        otherClient.generate(1).flatMap { blocks2 =>
          client.getBestBlockHash.flatMap { bestHash1 =>
            assert(bestHash1 == blocks1.head)
            otherClient.getBestBlockHash.flatMap { bestHash2 =>
              assert(bestHash2 == blocks2.head)
              client.addNode(otherClient.getDaemon.uri, "onetry").flatMap { _ =>
                Thread.sleep(1000)
                client.preciousBlock(bestHash2).flatMap { _ =>
                  client.getBestBlockHash.map { newBestHash =>
                    assert(newBestHash == blocks2.head)
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
    client.getNewAddress().flatMap { address =>
      fundBlockChainTransaction(client, address, Bitcoins(1.25)).flatMap {
        txid =>
          client.listAddressGroupings.flatMap { groupings =>
            val rpcAddress =
              groupings.find(vec => vec.head.address == address).get.head
            assert(rpcAddress.address == address)
            assert(rpcAddress.balance == Bitcoins(1.25))

            getFirstBlock().flatMap { block =>
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

  it should "be able to list utxos" in {
    client.listUnspent.flatMap { unspent =>
      getFirstBlock().flatMap { block =>
        val address = block.tx.head.vout.head.scriptPubKey.addresses.get.head
        val unspentMined =
          unspent.filter(addr => addr.address.contains(address))
        assert(unspentMined.length >= 100)
      }
    }
  }

  it should "be able to lock and unlock utxos as well as list locked utxos" in {
    client.listUnspent.flatMap { unspent =>
      val txid1 = unspent(0).txid
      val vout1 = unspent(0).vout
      val txid2 = unspent(1).txid
      val vout2 = unspent(1).vout
      val param = Vector(RpcOpts.LockUnspentOutputParameter(txid1, vout1),
                         RpcOpts.LockUnspentOutputParameter(txid2, vout2))
      client.lockUnspent(false, param).flatMap { success =>
        assert(success)
        client.listLockUnspent.flatMap { locked =>
          assert(locked.length == 2)
          assert(locked(0).txId == txid1)
          assert(locked(1).txId == txid2)
          client.lockUnspent(true, param).flatMap { success =>
            assert(success)
            client.listLockUnspent.map { newLocked =>
              assert(newLocked.isEmpty)
            }
          }
        }
      }
    }
  }

  it should "be able to get blockchain info" in {
    client.getBlockChainInfo.flatMap { info =>
      assert(info.chain == "regtest")
      client.getBestBlockHash.map(bestHash =>
        assert(info.bestblockhash == bestHash))
      assert(info.softforks.length >= 3)
      assert(info.bip9_softforks.keySet.size >= 2)
    }
  }

  it should "be able to create a raw transaction" in {
    client.generate(2).flatMap { blocks =>
      client.getBlock(blocks(0)).flatMap { block0 =>
        client.getBlock(blocks(1)).flatMap { block1 =>
          client.getTransaction(block0.tx(0)).flatMap { transaction0 =>
            client.getTransaction(block1.tx(0)).flatMap { transaction1 =>
              val input0 =
                TransactionOutPoint(transaction0.txid,
                                    UInt32(transaction0.blockindex.get))
              val input1 =
                TransactionOutPoint(transaction1.txid,
                                    UInt32(transaction1.blockindex.get))
              val sig: ScriptSignature = ScriptSignature.empty
              otherClient.getNewAddress().flatMap { address =>
                client
                  .createRawTransaction(
                    Vector(TransactionInput(input0, sig, UInt32(1)),
                           TransactionInput(input1, sig, UInt32(2))),
                    Map((address, Bitcoins(1))))
                  .map { transaction =>
                    assert(transaction.inputs(0).sequence == UInt32(1))
                    assert(transaction.inputs(1).sequence == UInt32(2))
                    assert(
                      transaction
                        .inputs(0)
                        .previousOutput
                        .txId
                        .flip == input0.txId)
                    assert(
                      transaction
                        .inputs(1)
                        .previousOutput
                        .txId
                        .flip == input1.txId)
                  }
              }
            }
          }
        }
      }
    }
  }

  it should "be able to fund a raw transaction" in {
    otherClient.getNewAddress().flatMap { address =>
      client
        .createRawTransaction(Vector.empty, Map(address -> Bitcoins(1)))
        .flatMap { transactionWithoutFunds =>
          client.fundRawTransaction(transactionWithoutFunds).flatMap {
            transactionResult =>
              val transaction = transactionResult.hex
              assert(transaction.inputs.length == 1)
              client
                .getRawTransaction(
                  transaction.inputs.head.previousOutput.txId.flip)
                .flatMap { inputTransaction =>
                  assert(
                    inputTransaction
                      .vout(transaction.inputs.head.previousOutput.vout.toInt)
                      .value
                      .satoshis
                      .toBigInt ==
                      transactionResult.fee.satoshis.toBigInt +
                        transaction.outputs(0).value.satoshis.toBigInt +
                        transaction.outputs(1).value.satoshis.toBigInt)
                }
          }
        }
    }
  }

  it should "be able to send from an account to an addresss" in {
    otherClient.getNewAddress().flatMap { address =>
      client.sendFrom("", address, Bitcoins(1)).flatMap { txid =>
        client.getTransaction(txid).map { transaction =>
          assert(transaction.amount == Bitcoins(-1))
          assert(transaction.details.head.address.contains(address))
        }
      }
    }
  }

  it should "be able to send to an address" in {
    otherClient.getNewAddress().flatMap { address =>
      client.sendToAddress(address, Bitcoins(1)).flatMap { txid =>
        client.getTransaction(txid).map { transaction =>
          assert(transaction.amount == Bitcoins(-1))
          assert(transaction.details.head.address.contains(address))
        }
      }
    }
  }

  it should "be able to send btc to many addresses" in {
    otherClient.getNewAddress().flatMap { address1 =>
      otherClient.getNewAddress().flatMap { address2 =>
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
    otherClient.getNewAddress().flatMap { address =>
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
    otherClient.getNewAddress().flatMap { address =>
      client
        .createRawTransaction(Vector(), Map(address -> Bitcoins(1)))
        .flatMap { tx =>
          recoverToSucceededIf[RuntimeException](
            client.abandonTransaction(tx.txIdBE))
        }
    }
  }

  it should "be able to find mem pool ancestors and descendants" in {
    client.generate(1)
    client.getNewAddress().flatMap { address =>
      fundMemPoolTransaction(client, address, Bitcoins(2)).flatMap { txid1 =>
        client.getRawMemPool.map { mempool =>
          assert(mempool.head == txid1)
        }
        client.getNewAddress().flatMap { address =>
          val input: TransactionInput =
            TransactionInput(TransactionOutPoint(txid1, UInt32(0)),
                             ScriptSignature.empty,
                             UInt32.max - UInt32.one)
          client
            .createRawTransaction(Vector(input), Map(address -> Bitcoins(1)))
            .flatMap { createdTx =>
              client.signRawTransaction(createdTx).flatMap { signedTx =>
                assert(signedTx.complete)
                client.sendRawTransaction(signedTx.hex, true).flatMap { txid2 =>
                  client.getMemPoolDescendants(txid1).map { descendants =>
                    assert(descendants.head == txid2)
                  }
                  client
                    .getMemPoolDescendantsVerbose(txid1)
                    .map { descendants =>
                      assert(descendants.head._1 == txid2)
                      assert(descendants.head._2.ancestorcount == 1)
                    }
                  client.getMemPoolAncestors(txid2).map { ancestors =>
                    assert(ancestors.head == txid1)
                  }
                  client
                    .getMemPoolAncestorsVerbose(txid2)
                    .map { ancestors =>
                      assert(ancestors.head._1 == txid1)
                      assert(ancestors.head._2.descendantcount == 2)
                    }
                }
              }
            }
        }
      }
    }
  }

  it should "be able to list transactions by receiving addresses" in {
    otherClient.getNewAddress().flatMap { address =>
      fundBlockChainTransaction(client, address, Bitcoins(1.5)).flatMap {
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
    client.getNewAddress().flatMap { address =>
      otherClient.importAddress(address).flatMap { _ =>
        fundBlockChainTransaction(client, address, Bitcoins(1.5)).flatMap {
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

  it should "be able to get a raw transaction using both rpcs available" in {
    getFirstBlock().flatMap { block =>
      val txid = block.tx.head.txid
      client.getRawTransaction(txid).flatMap { transaction1 =>
        client.getTransaction(txid).map { transaction2 =>
          assert(transaction1.txid == transaction2.txid)
          assert(transaction1.confirmations == transaction2.confirmations)
          assert(transaction1.hex == transaction2.hex)
          assert(transaction2.blockhash.contains(transaction1.blockhash))
          assert(transaction2.blocktime.contains(transaction1.blocktime))
        }
      }
    }
  }

  it should "be able to get utxo info" in {
    getFirstBlock().flatMap { block =>
      client.getTxOut(block.tx.head.txid, 0).map { info1 =>
        assert(info1.coinbase)
      }
    }
  }

  it should "be able to decode a raw transaction" in {
    createRawCoinbaseTransaction().flatMap { transaction =>
      client.decodeRawTransaction(transaction).map { rpcTransaction =>
        assert(rpcTransaction.txid == transaction.txIdBE)
        assert(rpcTransaction.locktime == transaction.lockTime)
        assert(rpcTransaction.size == transaction.size)
        assert(rpcTransaction.version == transaction.version.toInt)
        assert(rpcTransaction.vsize == transaction.vsize)
      }
    }
  }

  it should "be able to sign a raw transaction with wallet keys" in {
    createRawCoinbaseTransaction().flatMap { transaction =>
      client.signRawTransaction(transaction).map { signedTransaction =>
        assert(signedTransaction.complete)
      }
    }
  }

  it should "be able to send a raw transaction to the mem pool" in {
    createRawCoinbaseTransaction().flatMap { transaction =>
      client.signRawTransaction(transaction).flatMap { signedTransaction =>
        client
          .generate(100)
          .flatMap { _ => // Can't spend coinbase until depth 100
            client.sendRawTransaction(signedTransaction.hex, true).map {
              transactionHash =>
                succeed
            }
          }
      }
    }
  }

  it should "be able to get a raw transaction in serialized form from the mem pool" in {
    sendCoinbaseTransaction().flatMap { tx =>
      client.getRawTransactionRaw(tx.txid).map { transaction =>
        assert(transaction.txIdBE == tx.txid)
      }
    }
  }

  it should "be able to find a transaction sent to the mem pool" in {
    sendCoinbaseTransaction().flatMap { transaction =>
      client.getRawMemPool.map { memPool =>
        assert(memPool.length == 1)
        assert(memPool.head == transaction.txid)
      }
    }
  }

  it should "be able to find a verbose transaction in the mem pool" in {
    sendCoinbaseTransaction().flatMap { transaction =>
      client.getRawMemPoolWithTransactions.flatMap { memPool =>
        val txid = memPool.keySet.head
        assert(txid == transaction.txid)
        assert(memPool(txid).size > 0)
      }
    }
  }

  it should "be able to find a mem pool entry" in {
    sendCoinbaseTransaction().flatMap { transaction =>
      client.getMemPoolEntry(transaction.txid).map { memPoolEntry =>
        succeed
      }
    }
  }

  it should "be able to get mem pool info" in {
    client.generate(1).flatMap { _ =>
      client.getMemPoolInfo.flatMap { info =>
        assert(info.size == 0)
        sendCoinbaseTransaction().flatMap { _ =>
          client.getMemPoolInfo.map { newInfo =>
            assert(newInfo.size == 1)
          }
        }
      }
    }
  }

  it should "be able to prioritise a mem pool transaction" in {
    otherClient.getNewAddress().flatMap { address =>
      fundMemPoolTransaction(client, address, Bitcoins(3.2)).flatMap { txid =>
        client.getMemPoolEntry(txid).flatMap { entry =>
          assert(entry.fee == entry.modifiedfee)
          client.prioritiseTransaction(txid, Bitcoins(1).satoshis).flatMap {
            tt =>
              assert(tt)
              client.getMemPoolEntry(txid).map { newEntry =>
                assert(newEntry.fee == entry.fee)
                assert(newEntry.modifiedfee == newEntry.fee + Bitcoins(1))
              }
          }
        }
      }
    }
  }

  it should "be able to get a transaction" in {
    getFirstBlock().flatMap { block =>
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

  it should "be able to get tx out proof and verify it" in {
    getFirstBlock().flatMap { block =>
      client.getTxOutProof(Vector(block.tx.head.txid)).flatMap { merkle =>
        assert(merkle.transactionCount == UInt32(1))
        assert(merkle.hashes.length == 1)
        assert(merkle.hashes.head.flip == block.tx.head.txid)
        client.verifyTxOutProof(merkle).map { txids =>
          assert(block.tx.head.txid == txids.head)
        }
      }
    }
  }

  it should "be able to get the network hash per sec" in {
    client.getNetworkHashPS().map { hps =>
      assert(hps > 0)
    }
  }

  it should "be able to get an address from bitcoind" in {
    client.getNewAddress().map { address =>
      succeed
    }
  }

  it should "be able to get all addresses belonging to an account" in {
    client.getNewAddress().flatMap { address =>
      client.getAddressesByAccount("").map { addresses =>
        assert(addresses.contains(address))
      }
    }
  }

  it should "be able to get a new raw change address" in {
    client.getRawChangeAddress().map { address =>
      succeed
    }

    client.getRawChangeAddress(AddressType.Legacy).map { address =>
      succeed
    }

    client.getRawChangeAddress(AddressType.P2SHSegwit).map { address =>
      succeed
    }

    client.getRawChangeAddress(AddressType.Bech32).map { address =>
      succeed
    }
  }

  it should "be able to get the amount recieved by some address" in {
    client.getNewAddress().flatMap { address =>
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
      sendCoinbaseTransaction(client, client).flatMap { transaction =>
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

    walletClient.getConnectionCount.map { count =>
      assert(count == 0)
    }
  }

  it should "be able to get the best block hash" in {
    client.getBestBlockHash.map { hash =>
      succeed
    }
  }

  it should "be able to get the mining info" in {
    client.getMiningInfo.map { info =>
      assert(info.chain == "regtest")
    }
  }

  it should "be able to get the chain tips" in {
    client.getChainTips.map { tipArray =>
      succeed
    }
  }

  it should "be able to get the network info" in {
    client.getNetworkInfo.map { info =>
      assert(info.networkactive)
      assert(info.localrelay)
      assert(info.connections == 1)
    }
  }

  it should "be able to generate blocks" in {
    client.generate(3).map { blocks =>
      assert(blocks.length == 3)
    }
  }

  it should "be able to generate blocks to an address" in {
    otherClient.getNewAddress().flatMap { address =>
      client.generateToAddress(3, address).flatMap { blocks =>
        assert(blocks.length == 3)
        blocks.foreach { hash =>
          client.getBlockWithTransactions(hash).map { block =>
            assert(
              block.tx.head.vout.head.scriptPubKey.addresses.get.head == address)
          }
        }
        succeed
      }
    }
  }

  it should "be able to generate blocks and then get their serialized headers" in {
    client.generate(2).flatMap { blocks =>
      client.getBlockHeaderRaw(blocks(1)).map { header =>
        assert(header.previousBlockHashBE == blocks(0))
      }
    }
  }

  it should "be able to generate blocks and then get their headers" in {
    client.generate(2).flatMap { blocks =>
      client.getBlockHeader(blocks(0)).map { header =>
        assert(header.nextblockhash.contains(blocks(1)))
      }
      client.getBlockHeader(blocks(1)).map { header =>
        assert(header.previousblockhash.contains(blocks(0)))
        assert(header.nextblockhash.isEmpty)
      }
    }
  }

  it should "be able to get a block template" in {
    client.getBlockTemplate().map { template =>
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
        client.getBlockHash(count).map { hash =>
          assert(blocks(1) == hash)
        }
        client.getBlockHash(count - 1).map { hash =>
          assert(blocks(0) == hash)
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

  it should "be able to deactivate and activate the network" in {
    client.setNetworkActive(false).flatMap { _ =>
      client.getNetworkInfo.flatMap { info =>
        assert(!info.networkactive)
        client.setNetworkActive(true).flatMap { _ =>
          client.getNetworkInfo.map { newInfo =>
            assert(newInfo.networkactive)
          }
        }
      }
    }
  }

  it should "be able to get the difficulty on the network" in {
    client.getDifficulty.map { difficulty =>
      assert(difficulty > 0)
      assert(difficulty < 1)
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

  it should "be able to ping" in {
    client.ping().map(_ => succeed)
  }

  it should "be able to validate a bitcoin address" in {
    otherClient.getNewAddress().flatMap { address =>
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

  it should "be able to get the memory info" in {
    client.getMemoryInfo.map { info =>
      assert(info.locked.used > 0)
      assert(info.locked.free > 0)
      assert(info.locked.total > 0)
      assert(info.locked.locked > 0)
      assert(info.locked.chunks_used > 0)
    }
  }

  it should "be able to get network statistics" in {
    client.getNetTotals.map { stats =>
      assert(stats.timemillis.toBigInt > 0)
      assert(stats.totalbytesrecv > 0)
      assert(stats.totalbytessent > 0)
    }
  }

  it should "be able to create a multi sig address" in {
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val ecPrivKey2 = ECPrivateKey.freshPrivateKey

    val pubKey1 = ecPrivKey1.publicKey
    val pubKey2 = ecPrivKey2.publicKey

    client.createMultiSig(2, Vector(pubKey1, pubKey2)).map { result =>
      succeed
    }
  }

  it should "be able to add a multi sig address to the wallet" in {
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val pubKey1 = ecPrivKey1.publicKey

    client.getNewAddress(addressType = AddressType.Legacy).flatMap { address =>
      client
        .addMultiSigAddress(2,
                            Vector(Left(pubKey1),
                                   Right(address.asInstanceOf[P2PKHAddress])))
        .map { multisig =>
          succeed
        }
    }
  }

  it should "be able to decode a reedem script" in {
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val pubKey1 = ecPrivKey1.publicKey

    client.getNewAddress(addressType = AddressType.Legacy).flatMap { address =>
      client
        .addMultiSigAddress(2,
                            Vector(Left(pubKey1),
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
    client.getNewAddress().flatMap { address =>
      client.dumpPrivKey(address).map { key =>
        succeed
      }
    }
  }

  it should "be able to import a private key" in {
    val ecPrivateKey = ECPrivateKey.freshPrivateKey
    val publicKey = ecPrivateKey.publicKey
    val address = P2PKHAddress(publicKey, networkParam)

    client.importPrivKey(ecPrivateKey, rescan = false).flatMap { _ =>
      client.dumpPrivKey(address).map { key =>
        assert(key == ecPrivateKey)
      }
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

  it should "be able to import a public key" in {
    val pubKey = ECPublicKey.freshPublicKey

    client.importPubKey(pubKey).map { _ =>
      succeed
    }
  }

  it should "be able to sign a message and verify that signature" in {
    val message = "Never gonna give you up\nNever gonna let you down\n..."
    client.getNewAddress(addressType = AddressType.Legacy).flatMap { address =>
      client.signMessage(address.asInstanceOf[P2PKHAddress], message).flatMap {
        signature =>
          client
            .verifyMessage(address.asInstanceOf[P2PKHAddress],
                           signature,
                           message)
            .map { validity =>
              assert(validity)
            }
      }
    }
  }

  it should "be able to sign a message with a private key and verify that signature" in {
    val message = "Never gonna give you up\nNever gonna let you down\n..."
    val privKey = ECPrivateKey.freshPrivateKey
    val address = P2PKHAddress(privKey.publicKey, networkParam)

    client.signMessageWithPrivKey(privKey, message).flatMap { signature =>
      client.verifyMessage(address, signature, message).map { validity =>
        assert(validity)
      }
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
                                         UInt32(0))
            ),
            false
          )
          .flatMap { result =>
            assert(result.length == 2)
            assert(result(0).success)
            assert(result(1).success)
          }
      }
  }

  it should "be able to dump the wallet" in {
    client
      .dumpWallet(client.getDaemon.authCredentials.datadir + "/test.dat")
      .map { result =>
        assert(result.filename.exists)
        assert(result.filename.isFile)
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

  it should "be able to change the wallet password" in {
    walletClient.walletLock().flatMap { _ =>
      walletClient.walletPassphraseChange(password, "new_password").flatMap {
        _ =>
          password = "new_password"
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
  }

  it should "be able to import a wallet" in {
    client.getNewAddress().flatMap { address =>
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
                  .flatMap { key =>
                    succeed
                  }
              }
          }
        }
    }
  }

  it should "be able to list wallets" in {
    client.listWallets.map { wallets =>
      assert(wallets == Vector("wallet.dat"))
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
        assert(time1 > time2)
      }
    }
  }

  it should "be able to sign a raw transaction" in {
    client.getNewAddress().flatMap { address =>
      client.validateAddress(address).flatMap { addressInfo =>
        client
          .addMultiSigAddress(1, Vector(Left(addressInfo.pubkey.get)))
          .flatMap { multisig =>
            fundBlockChainTransaction(client, multisig.address, Bitcoins(1.2))
              .flatMap { txid =>
                client.getTransaction(txid).flatMap { rawTx =>
                  client.decodeRawTransaction(rawTx.hex).flatMap { tx =>
                    val output =
                      tx.vout.find(output => output.value == Bitcoins(1.2)).get
                    val input = TransactionInput(
                      TransactionOutPoint(txid, UInt32(output.n)),
                      P2SHScriptSignature(multisig.redeemScript.hex),
                      UInt32.max - UInt32.one)
                    client.getNewAddress().flatMap { newAddress =>
                      client
                        .createRawTransaction(Vector(input),
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
                                  Bitcoins(1.2)))
                            )
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
    client.getNewAddress().flatMap { address1 =>
      otherClient.getNewAddress().flatMap { address2 =>
        client.validateAddress(address1).flatMap { address1Info =>
          otherClient.validateAddress(address2).flatMap { address2Info =>
            client
              .addMultiSigAddress(2,
                                  Vector(Left(address1Info.pubkey.get),
                                         Left(address2Info.pubkey.get)))
              .flatMap { multisig =>
                otherClient
                  .addMultiSigAddress(2,
                                      Vector(Left(address1Info.pubkey.get),
                                             Left(address2Info.pubkey.get)))
                  .flatMap { _ =>
                    client.validateAddress(multisig.address).flatMap {
                      multisigInfo =>
                        fundBlockChainTransaction(client,
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
                                    TransactionOutPoint(txid, UInt32(output.n)),
                                    P2SHScriptSignature(
                                      multisig.redeemScript.hex),
                                    UInt32.max - UInt32.one)
                                  client.getNewAddress().flatMap { address =>
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
                                                Bitcoins(1.2)))
                                          )
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
                                                    Bitcoins(1.2)))
                                              )
                                              .flatMap { partialTx2 =>
                                                assert(!partialTx2.complete)
                                                assert(partialTx2.hex != ctx)
                                                client
                                                  .combineRawTransaction(
                                                    Vector(partialTx1.hex,
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
    otherClient.getNewAddress().flatMap { address =>
      client.listUnspent.flatMap { unspent =>
        val output = unspent.find(output => output.amount.toBigDecimal > 1).get
        val input: TransactionInput = TransactionInput(
          TransactionOutPoint(output.txid, UInt32(output.vout)),
          ScriptSignature.empty,
          UInt32.max - UInt32(2))
        client.getRawChangeAddress().flatMap { changeAddress =>
          client
            .createRawTransaction(
              Vector(input),
              Map(address -> Bitcoins(0.5),
                  changeAddress -> Bitcoins(output.amount.toBigDecimal - 0.55)))
            .flatMap { ctx =>
              client.signRawTransaction(ctx).flatMap { stx =>
                client.sendRawTransaction(stx.hex, true).flatMap { txid =>
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

  it should "be able to abort a rescan of the blockchain" in {
    recoverToSucceededIf[RuntimeException](client.rescanBlockChain())
    client.abortRescan().map { _ =>
      succeed
    }
  }

  it should "be able to get and set the account for a given address" in {
    val account1 = "account_1"
    val account2 = "account_2"
    client.getNewAddress(account1).flatMap { address =>
      client.getAccount(address).flatMap { acc1 =>
        assert(acc1 == account1)
        client.setAccount(address, account2).flatMap { _ =>
          client.getAccount(address).map { acc2 =>
            assert(acc2 == account2)
          }
        }
      }
    }
  }

  it should "be able to get an account's address" in {
    val account = "a_new_account"
    client.getAccountAddress(account).flatMap { address =>
      client.getAccount(address).map { result =>
        assert(result == account)
      }
    }
  }

  it should "be able to get the amount received by an account and list amounts received by all accounts" in {
    val account = "another_new_account"
    val emptyAccount = "empty_account"
    client.getNewAddress(account).flatMap { address =>
      fundBlockChainTransaction(client, address, Bitcoins(1.5)).flatMap {
        txid =>
          client.getReceivedByAccount(account).flatMap { amount =>
            assert(amount == Bitcoins(1.5))
            client.listReceivedByAccount().map { list =>
              assert(
                list
                  .find(acc => acc.account == account)
                  .get
                  .amount == Bitcoins(1.5))
              assert(
                list
                  .find(acc => acc.account == "")
                  .get
                  .amount > Bitcoins(0))
              assert(!list.exists(acc => acc.account == emptyAccount))
            }
            client.listAccounts().map { map =>
              assert(map(account) == Bitcoins(1.5))
              assert(map("") > Bitcoins(0))
              assert(!map.keySet.contains(emptyAccount))
            }
          }
      }
    }
  }

  it should "be able to move funds from one account to another" in {
    val account = "move_account"
    client.move("", account, Bitcoins(1)).flatMap { success =>
      assert(success)
      client.listAccounts().map { map =>
        assert(map(account) == Bitcoins(1))
      }
    }
  }

  it should "be able to add and remove a node" in {
    otherClient.addNode(walletClient.getDaemon.uri, "add").flatMap { _ =>
      Thread.sleep(10000)
      otherClient.getAddedNodeInfo(walletClient.getDaemon.uri).flatMap { info =>
        assert(info.length == 1)
        assert(info.head.addednode == walletClient.getDaemon.uri)
        assert(info.head.connected.contains(true))

        otherClient.addNode(walletClient.getDaemon.uri, "remove").flatMap { _ =>
          otherClient.getAddedNodeInfo.map { newInfo =>
            assert(newInfo.isEmpty)
          }
        }
      }
    }
  }

  it should "be able to add and disconnect a node" in {
    otherClient.addNode(walletClient.getDaemon.uri, "add").flatMap { _ =>
      Thread.sleep(1000)
      otherClient.getAddedNodeInfo(walletClient.getDaemon.uri).flatMap { info =>
        assert(info.head.connected.contains(true))

        otherClient.disconnectNode(walletClient.getDaemon.uri).flatMap { _ =>
          Thread.sleep(1000)
          otherClient.getAddedNodeInfo(walletClient.getDaemon.uri).map {
            newInfo =>
              assert(newInfo.head.connected.contains(false))
          }
        }
      }
    }
  }

  override def afterAll(): Unit = {
    client.stop().map(println)
    otherClient.stop().map(println)
    walletClient.stop().map(println)
    pruneClient.stop().map(println)
    if (TestUtil.deleteTmpDir(client.getDaemon.authCredentials.datadir))
      println("Temp bitcoin directory deleted")
    if (TestUtil.deleteTmpDir(otherClient.getDaemon.authCredentials.datadir))
      println("Temp bitcoin directory deleted")
    if (TestUtil.deleteTmpDir(walletClient.getDaemon.authCredentials.datadir))
      println("Temp bitcoin directory deleted")
    if (TestUtil.deleteTmpDir(pruneClient.getDaemon.authCredentials.datadir))
      println("Temp bitcoin directory deleted")
  }
}
