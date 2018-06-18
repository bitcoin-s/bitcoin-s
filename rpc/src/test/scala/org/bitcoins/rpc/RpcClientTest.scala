package org.bitcoins.rpc

import java.io.File
import java.lang.RuntimeException
import java.net.URI

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput, TransactionOutPoint}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.{RpcClient, RpcOpts}
import org.scalatest.{AsyncFlatSpec, BeforeAndAfter, BeforeAndAfterAll}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.protocol.script.{EmptyScriptSignature, ScriptSignature}
import org.bitcoins.rpc.jsonmodels.{GetTransactionResult, GetWalletInfoResult, MultiSigResult, RpcAddress}
import org.scalatest.concurrent.ScalaFutures

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

  val client = new RpcClient(TestUtil.instance(
    networkParam.port + 5,
    networkParam.rpcPort + 5)) // Change this back
  val otherClient = new RpcClient(
    TestUtil.instance(networkParam.port + 10, networkParam.rpcPort + 10))

  val walletClient = new RpcClient(
    TestUtil.instance(networkParam.port + 20, networkParam.rpcPort + 20))

  val logger = BitcoinSLogger.logger

  var password = "password"

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
  }

  behavior of "RpcClient"

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
    client.getAddedNodeInfo().flatMap { info =>
      assert(info.length == 1)
      assert(info.head.addednode == otherClient.getDaemon.uri)
      assert(info.head.connected.contains(true))
    }
  }
/* TODO: Reconnect after ban is removed
  it should "be able to ban and clear the ban of a subnet" in {
    val loopBack = URI.create("http://127.0.0.1")
    client.setBan(loopBack, "add").flatMap { _ =>
      client.listBanned.flatMap { list =>
        assert(list.length == 1)
        assert(list.head.address.getAuthority == loopBack.getAuthority)
        assert(list.head.banned_until - list.head.ban_created == UInt32(86400))

        client.setBan(loopBack, "remove").flatMap { _ =>
          client.listBanned.map { newList =>
            assert(newList.isEmpty)
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

          client.clearBanned.flatMap { _ =>
            client.listBanned.map { newList =>
              assert(newList.isEmpty)
            }
          }
        }
      }
    }
  }
*/
  it should "be able to list address groupings" in {
    client.getNewAddress().flatMap { address =>
      client.createRawTransaction(Vector(), Map(address -> Bitcoins(1.25))).flatMap { createdTransaction =>
        client.fundRawTransaction(createdTransaction).flatMap { fundedTransaction =>
          client.signRawTransaction(fundedTransaction.hex).flatMap { signedTransaction =>
            client.sendRawTransaction(signedTransaction.hex).flatMap { txid =>
              client.generate(1).flatMap { _ =>
                client.listAddressGroupings.flatMap { groupings =>
                  val rpcAddresss = groupings.last.head
                  assert(rpcAddresss.address == address)
                  assert(rpcAddresss.balance == Bitcoins(1.25))

                  client.getBlockHash(1).flatMap { hash =>
                    client.getBlockWithTransactions(hash).map { block =>
                      val firstAddress = block.tx.head.vout.head.scriptPubKey.addresses.get.head
                      assert(groupings.max(Ordering.by[Vector[RpcAddress], BigDecimal](addr => addr.head.balance.toBigDecimal)).head.address == firstAddress)
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

  it should "be able to list utxos" in {
    client.listUnspent().flatMap { unspent =>
      client.getBlockHash(1).flatMap { hash =>
        client.getBlockWithTransactions(hash).flatMap { block =>
          val address = block.tx.head.vout.head.scriptPubKey.addresses.get.head
          val unspentMined = unspent.filter(addr => addr.address.contains(address))
          assert(unspentMined.length >= 100)
        }
      }
    }
  }

  it should "be able to lock and unlock utxos as well as list locked utxos" in {
    client.listUnspent().flatMap { unspent =>
      val txid1 = unspent(0).txid
      val vout1 = unspent(0).vout
      val txid2 = unspent(1).txid
      val vout2 = unspent(1).vout
      val param = Vector(RpcOpts.LockUnspentOutputParameter(txid1, vout1), RpcOpts.LockUnspentOutputParameter(txid2, vout2))
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
              client.getNewAddress().flatMap { address =>
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
      client.createRawTransaction(Vector.empty, Map(address -> Bitcoins(1))).flatMap { transactionWithoutFunds =>
        client.fundRawTransaction(transactionWithoutFunds).flatMap { transactionResult =>
          val transaction = transactionResult.hex
          assert(transaction.inputs.length == 1)
          client.getRawTransaction(transaction.inputs.head.previousOutput.txId.flip).flatMap { inputTransaction =>
            assert(inputTransaction.vout(transaction.inputs.head.previousOutput.vout.toInt).value.satoshis.toBigInt ==
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
        client.sendMany(Map(address1 -> Bitcoins(1), address2 -> Bitcoins(2))).flatMap { txid =>
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
      client.createRawTransaction(Vector(), Map(address -> Bitcoins(1))).flatMap { tx =>
        recoverToSucceededIf[RuntimeException](client.abandonTransaction(tx.txIdBE))
      }
    }
  }

  it should "be able to find mem pool ancestors and descendants" in {
    client.generate(1)
    client.getNewAddress().flatMap { address1 =>
      client.createRawTransaction(Vector(), Map(address1 -> Bitcoins(2))).flatMap { createdTransaction1 =>
        client.fundRawTransaction(createdTransaction1).flatMap { fundedTransaction1 =>
          client.signRawTransaction(fundedTransaction1.hex).flatMap { signedTransaction1 =>
            assert(signedTransaction1.complete)
            client.sendRawTransaction(signedTransaction1.hex).flatMap { txid1 =>
              client.getRawMemPool.map { mempool => assert(mempool.head == txid1)}
              client.getNewAddress().flatMap { address2 =>
                val input: TransactionInput = TransactionInput(TransactionOutPoint(txid1, UInt32(0)), ScriptSignature.empty, UInt32.max-UInt32.one)
                client.createRawTransaction(Vector(input), Map(address2 -> Bitcoins(1))).flatMap { createdTransaction2 =>
                  client.signRawTransaction(createdTransaction2).flatMap { signedTransaction2 =>
                    assert(signedTransaction2.complete)
                    client.sendRawTransaction(signedTransaction2.hex, true).flatMap { txid2 =>
                      client.getMemPoolDescendants(txid1).map { descendants =>
                        assert(descendants.head == txid2)
                      }
                      client.getMemPoolDescendantsVerbose(txid1).map { descendants =>
                        assert(descendants.head._1 == txid2)
                        assert(descendants.head._2.ancestorcount == 1)
                      }
                      client.getMemPoolAncestors(txid2).map { ancestors =>
                        assert(ancestors.head == txid1)
                      }
                      client.getMemPoolAncestorsVerbose(txid2).map { ancestors =>
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
      }
    }
  }

  it should "be able to list transactions by receiving addresses" in {
    client.getNewAddress().flatMap { address =>
      client.createRawTransaction(Vector(), Map(address -> Bitcoins(1.5))).flatMap { createdTransaction =>
        client.fundRawTransaction(createdTransaction).flatMap { fundedTransaction =>
          client.signRawTransaction(fundedTransaction.hex).flatMap { signedTransaction =>
            client.sendRawTransaction(signedTransaction.hex).flatMap { txid =>
              client.generate(1).flatMap { _ =>
                client.listReceivedByAddress().map { receivedList =>
                  val entryList = receivedList.filter(entry => entry.address == address)
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
        }
      }
    }
  }

  it should "be able to import an address" in {
    client.getNewAddress().flatMap { address =>
      otherClient.importAddress(address).flatMap { _ =>
        client.createRawTransaction(Vector(), Map(address -> Bitcoins(1.5))).flatMap { createdTransaction =>
          client.fundRawTransaction(createdTransaction).flatMap { fundedTransaction =>
            client.signRawTransaction(fundedTransaction.hex).flatMap { signedTransaction =>
              client.sendRawTransaction(signedTransaction.hex).flatMap { txid =>
                client.generate(1).flatMap { _ =>
                  otherClient.listReceivedByAddress(includeWatchOnly = true).flatMap { list =>
                    val entryList = list.filter(addr => addr.involvesWatchonly.contains(true))
                    assert(entryList.length == 1)
                    val entry = entryList.head
                    assert(entry.address == address)
                    assert(entry.involvesWatchonly.contains(true))
                    assert(entry.amount == Bitcoins(1.5))
                    assert(entry.txids.head == txid)
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  private def createRawTransaction: Future[Transaction] = {
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
              client.getNewAddress().flatMap { address =>
                client.createRawTransaction(
                  Vector(TransactionInput(input0, sig, UInt32(1)),
                         TransactionInput(input1, sig, UInt32(2))),
                  Map((address, Bitcoins(1))))
              }
            }
          }
        }
      }
    }
  }

  it should "be able to get a raw transaction using both rpcs available" in {
    client.getBlockHash(1).flatMap { hash =>
      client.getBlock(hash).flatMap { block =>
        val txid = block.tx.head
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
  }

  it should "be able to get utxo info" in {
    client.getBlockHash(1).flatMap { hash =>
      client.getBlock(hash).flatMap { block =>
        client.getTxOut(block.tx.head, 0).map { info1 =>
          assert(info1.coinbase)
        }
      }
    }
  }

  it should "be able to decode a raw transaction" in {
    val transactionF = createRawTransaction
    transactionF.flatMap { transaction =>
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
    val transactionF = createRawTransaction
    transactionF.flatMap { transaction =>
      client.signRawTransaction(transaction).map { signedTransaction =>
        assert(signedTransaction.complete)
      }
    }
  }

  it should "be able to send a raw transaction to the mem pool" in {
    val transactionF = createRawTransaction
    transactionF.flatMap { transaction =>
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

  private def sendTransaction: Future[GetTransactionResult] = {
    val transactionF = createRawTransaction
    transactionF.flatMap { transaction =>
      client.signRawTransaction(transaction).flatMap { signedTransaction =>
        client
          .generate(100)
          .flatMap { _ => // Can't spend coinbase until depth 100
            client.sendRawTransaction(signedTransaction.hex, true).flatMap {
              transactionHash =>
                client.getTransaction(transactionHash)
            }
          }
      }
    }
  }

  it should "be able to get a raw transaction in serialized form from the mem pool" in {
    sendTransaction.flatMap { tx =>
      client.getRawTransactionRaw(tx.txid).map { transaction =>
        assert(transaction.txIdBE == tx.txid)
      }
    }
  }

  it should "be able to find a transaction sent to the mem pool" in {
    val transactionF = sendTransaction
    transactionF.flatMap { transaction =>
      client.getRawMemPool.map { memPool =>
        assert(memPool.length == 1)
        assert(memPool.head == transaction.txid)
      }
    }
  }

  it should "be able to find a verbose transaction in the mem pool" in {
    sendTransaction.flatMap { transaction =>
      client.getRawMemPoolWithTransactions.flatMap { memPool =>
        val txid = memPool.keySet.head
        assert(txid == transaction.txid)
        assert(memPool(txid).size > 0)
      }
    }
  }

  it should "be able to find a mem pool entry" in {
    val transactionF = sendTransaction
    transactionF.flatMap { transaction =>
      client.getMemPoolEntry(transaction.txid).map { memPoolEntry =>
        succeed
      }
    }
  }

  it should "be able to get mem pool info" in {
    client.generate(1).flatMap { _ =>
      client.getMemPoolInfo.flatMap { info =>
        assert(info.size == 0)
        sendTransaction.flatMap { _ =>
          client.getMemPoolInfo.map { newInfo =>
            assert(newInfo.size == 1)
          }
        }
      }
    }
  }

  it should "be able to get a transaction" in { // Assumes a block
    client.getBlockHash(1).flatMap { blockHash =>
      client.getBlock(blockHash).flatMap { block =>
        client.getTransaction(block.tx.head).flatMap { tx =>
          assert(tx.txid == block.tx.head)
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
  }

  it should "be able to get tx out proof and verify it" in {
    client.getBlockHash(1).flatMap { hash =>
      client.getBlock(hash).flatMap { block =>
        client.getTxOutProof(Vector(block.tx.head)).flatMap { merkle =>
          assert(merkle.transactionCount == UInt32(1))
          assert(merkle.hashes.length == 1)
          assert(merkle.hashes.head.flip == block.tx.head)
          client.verifyTxOutProof(merkle).map { txids =>
            assert(block.tx.head == txids.head)
          }
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
    val addressF = client.getNewAddress()
    addressF.map { address =>
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

    client.getRawChangeAddress(Some("legacy")).map { address =>
      succeed
    }

    client.getRawChangeAddress(Some("p2sh-segwit")).map { address =>
      succeed
    }

    client.getRawChangeAddress(Some("bech32")).map { address =>
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

  it should "be able to get the unconfirmed balance" in {
    client.getUnconfirmedBalance.flatMap { balance =>
      assert(balance == Bitcoins(0))
      sendTransaction.flatMap { transaction =>
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
    val blockCountF = client.getBlockCount
    blockCountF.flatMap { count =>
      assert(count >= 0)
      otherClient.getBlockCount.map { otherCount =>
        assert(count == otherCount)
      }
    }
  }

  it should "be able to get the connection count" in {
    val connectionCountF = client.getConnectionCount
    connectionCountF.map { count =>
      assert(count == 1)
    }

    walletClient.getConnectionCount.map { count =>
      assert(count == 0)
    }
  }

  it should "be able to get the best block hash" in {
    val bestHashF = client.getBestBlockHash
    bestHashF.map { hash =>
      succeed
    }
  }

  it should "be able to get the mining info" in {
    val miningInfoF = client.getMiningInfo
    miningInfoF.map { info =>
      assert(info.chain == "regtest")
    }
  }

  it should "be able to get the chain tips" in { // Is there more to test here?
    val chainTipsF = client.getChainTips
    chainTipsF.map { tipArray =>
      succeed
    }
  }

  it should "be able to get the network info" in {
    val networkInfoF = client.getNetworkInfo
    networkInfoF.map { info =>
      assert(info.networkactive)
      assert(info.localrelay)
    }
  }

  it should "be able to generate blocks" in {
    val blocksF = client.generate(3)
    blocksF.map { blocks =>
      assert(blocks.length == 3)
    }
  }

  it should "be able to generate blocks to an address" in {
    client.getNewAddress().flatMap { address =>
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
    val blocksF = client.generate(2)
    blocksF.flatMap { blocks =>
      val headerF1 = client.getBlockHeaderRaw(blocks(1))
      headerF1.map { header =>
        assert(header.previousBlockHashBE == blocks(0))
      }
    }
  }

  it should "be able to generate blocks and then get their headers" in {
    val blocksF = client.generate(2)
    blocksF.flatMap { blocks =>
      val headerF0 = client.getBlockHeader(blocks(0))
      headerF0.map { header =>
        assert(header.nextblockhash.contains(blocks(1)))
      }
      val headerF1 = client.getBlockHeader(blocks(1))
      headerF1.map { header =>
        assert(header.previousblockhash.contains(blocks(0)))
        assert(header.nextblockhash.isEmpty)
      }
    }
  }

  it should "be able to get the balance" in {
    val balanceF = client.getBalance
    balanceF.flatMap { balance =>
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
    client.ping.map(_ => succeed)
  }

  it should "be able to validate a bitcoin address" in {
    client.getNewAddress().flatMap { address =>
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

    client.getNewAddress(addressType = Some("legacy")).flatMap { address =>
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

    client.getNewAddress(addressType = Some("legacy")).flatMap { address =>
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
    client.getNewAddress(addressType = Some("legacy")).flatMap { address =>
      client.dumpPrivKey(address.asInstanceOf[P2PKHAddress]).map { key =>
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
    }
  }

  it should "be able to sign a message and verify that signature" in {
    val message = "Never gonna give you up\nNever gonna let you down\n..."
    client.getNewAddress(addressType = Some("legacy")).flatMap { address =>
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
    walletClient.walletLock.flatMap { _ =>
      walletClient.walletPassphrase(password, 1000).flatMap { _ =>
        walletClient.getWalletInfo.flatMap { info =>
          assert(info.unlocked_until.nonEmpty)
          assert(info.unlocked_until.get > 0)

          walletClient.walletLock.flatMap { _ =>
            walletClient.getWalletInfo.map { newInfo =>
              assert(newInfo.unlocked_until.contains(0))
            }
          }
        }
      }
    }
  }

  it should "be able to change the wallet password" in {
    walletClient.walletLock.flatMap { _ =>
      walletClient.walletPassphraseChange(password, "new_password").flatMap {
        _ =>
          password = "new_password"
          walletClient.walletPassphrase(password, 1000).flatMap { _ =>
            walletClient.getWalletInfo.flatMap { info =>
              assert(info.unlocked_until.nonEmpty)
              assert(info.unlocked_until.get > 0)

              walletClient.walletLock.flatMap { _ =>
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
    client.getNewAddress(addressType = Some("legacy")).flatMap { address =>
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
                  .dumpPrivKey(address.asInstanceOf[P2PKHAddress])
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
    client.getChainTxStats().map { stats =>
      assert(stats.time > UInt32(0))
      assert(stats.txcount > 0)
      assert(stats.window_block_count > 0)
    }
  }

  it should "be able to save the mem pool to disk" in {
    val regTest = new File(client.getDaemon.authCredentials.datadir + "/regtest")
    assert(regTest.isDirectory)
    assert(!regTest.list().contains("mempool.dat"))
    client.saveMemPool.map { _ =>
      assert(regTest.list().contains("mempool.dat"))
    }
  }

  it should "be able to get and set the logging configuration" in {
    client.logging().flatMap { info =>
      info.keySet.foreach(category => assert(info(category) == 1))
      client.logging(exclude = Some(Vector("qt"))).map { info =>
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

  /*
  it should "be able to combine raw transactions" in {
    client.getNewAddress().flatMap { address =>
      client.createRawTransaction(Vector(), Map(address -> Bitcoins(2.5))).flatMap { ctx1 =>
        client.fundRawTransaction(ctx1).flatMap { ftx1 =>
          client.signRawTransaction(ftx1.hex).flatMap { tx1 =>
            client.createRawTransaction(Vector(), Map(address -> Bitcoins(3.5))).flatMap { ctx2 =>
              client.fundRawTransaction(ctx2).flatMap { ftx2 =>
                client.signRawTransaction(ftx2.hex).flatMap { tx2 =>
                  client.combineRawTransaction(Vector(tx1.hex, tx2.hex)).flatMap { tx =>
                    client.decodeRawTransaction(tx).map { decoded =>
                      assert(decoded.vout.exists(output => output.value == Bitcoins(2.5)))
                      assert(decoded.vout.exists(output => output.value == Bitcoins(3.5)))
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }*/

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
    client.abortRescan.map { _ =>
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
      client.createRawTransaction(Vector(), Map(address -> Bitcoins(1.5))).flatMap { ctx =>
        client.fundRawTransaction(ctx).flatMap { ftx =>
          client.signRawTransaction(ftx.hex).flatMap { stx =>
            client.sendRawTransaction(stx.hex).flatMap { txid =>
              client.generate(1).flatMap { _ =>
                client.getReceivedByAccount(account).flatMap { amount =>
                  assert(amount == Bitcoins(1.5))
                  client.listReceivedByAccount().map { list =>
                    assert(list.filter(acc => acc.account == account).head.amount == Bitcoins(1.5))
                    assert(list.filter(acc => acc.account == "").head.amount > Bitcoins(0))
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
      otherClient.getAddedNodeInfo(Some(walletClient.getDaemon.uri)).flatMap {
        info =>
          assert(info.length == 1)
          assert(info.head.addednode == walletClient.getDaemon.uri)
          assert(info.head.connected.contains(true))

          otherClient.addNode(walletClient.getDaemon.uri, "remove").flatMap {
            _ =>
              otherClient.getAddedNodeInfo().map { newInfo =>
                assert(newInfo.isEmpty)
              }
          }
      }
    }
  }

  it should "be able to add and disconnect a node" in {
    otherClient.addNode(walletClient.getDaemon.uri, "add").flatMap { _ =>
      Thread.sleep(1000)
      otherClient.getAddedNodeInfo(Some(walletClient.getDaemon.uri)).flatMap {
        info =>
          assert(info.head.connected.contains(true))

          otherClient.disconnectNode(walletClient.getDaemon.uri).flatMap { _ =>
            Thread.sleep(1000)
            otherClient.getAddedNodeInfo(Some(walletClient.getDaemon.uri)).map {
              newInfo =>
                assert(newInfo.head.connected.contains(false))
            }
          }
      }
    }
  }

  override def afterAll(): Unit = {
    client.stop.map(println)
    otherClient.stop.map(println)
    walletClient.stop.map(println)
    if (TestUtil.deleteTmpDir(client.getDaemon.authCredentials.datadir))
      println("Temp bitcoin directory deleted")
    if (TestUtil.deleteTmpDir(otherClient.getDaemon.authCredentials.datadir))
      println("Temp bitcoin directory deleted")
    if (TestUtil.deleteTmpDir(walletClient.getDaemon.authCredentials.datadir))
      println("Temp bitcoin directory deleted")
  }
}
