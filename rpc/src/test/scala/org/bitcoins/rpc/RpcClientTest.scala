package org.bitcoins.rpc

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.RpcClient
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.rpc.jsonmodels.GetTransactionResult

import scala.concurrent.Future

// Need to test encryptwallet, walletpassphrase, walletpassphrasechange on startup
// And walletlock, stop on close
class RpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system = ActorSystem()
  implicit val m = ActorMaterializer()
  implicit val ec = m.executionContext
  implicit val networkParam = TestUtil.network

  val client = new RpcClient(
    TestUtil.instance(networkParam.port, networkParam.rpcPort))
  val logger = BitcoinSLogger.logger

  override def beforeAll(): Unit = {
    println("Temp bitcoin directory created")
    client.start()
    println("Bitcoin server starting")
    Thread.sleep(1000)
  }

  behavior of "RpcClient"

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
                    Map((address.value, Bitcoins(1))))
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
                  Map((address.value, Bitcoins(0.0000001))))
              }
            }
          }
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
                assert(true)
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
        assert(true)
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

  it should "be able to get the network hash per sec" in {
    client.getNetworkHashPS().map { hps =>
      assert(hps > 0)
    }
  }

  it should "be able to get an address from bitcoind" in {
    val addressF = client.getNewAddress()
    addressF.map { address =>
      assert(true)
    }
  }

  it should "be able to get a new raw change address" in {
    client.getRawChangeAddress().map { address =>
      assert(true)
    }

    client.getRawChangeAddress(Some("legacy")).map { address =>
      assert(true)
    }

    client.getRawChangeAddress(Some("p2sh-segwit")).map { address =>
      assert(true)
    }

    client.getRawChangeAddress(Some("bech32")).map { address =>
      assert(true)
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
    blockCountF.map { count =>
      assert(count >= 0)
    }
  }

  it should "be able to get the connection count" in {
    val connectionCountF = client.getConnectionCount
    connectionCountF.map { count =>
      assert(count == 0)
    }
  }

  it should "be able to get the best block hash" in {
    val bestHashF = client.getBestBlockHash
    bestHashF.map { hash =>
      assert(true)
    }
  }

  it should "be able to get the mining info" in {
    val miningInfoF = client.getMiningInfo
    miningInfoF.map { info =>
      assert(info.chain == "regtest")
    }
  }

  it should "be able to get the chain tips" in {
    val chainTipsF = client.getChainTips
    chainTipsF.map { tipArray =>
      tipArray.foreach { tip =>
        assert(tip.status == "active")
      }
      assert(true)
    }
  }

  it should "be able to get the network info" in {
    val networkInfoF = client.getNetworkInfo
    networkInfoF.map { info =>
      assert(info.networkactive)
      assert(info.connections == 0)
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
        assert(true)
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
    client.ping.map(_ => assert(true))
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
      // Regtest so nothing sent over network
      assert(stats.totalbytesrecv == 0)
      assert(stats.totalbytessent == 0)
    }
  }

  override def afterAll(): Unit = {
    client.stop.map(println)
    if (TestUtil.deleteTmpDir(client.getInstance.authCredentials.datadir))
      println("Temp bitcoin directory deleted")
  }
}
