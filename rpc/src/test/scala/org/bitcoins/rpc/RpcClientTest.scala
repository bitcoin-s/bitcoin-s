package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput, TransactionOutPoint}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.RpcClient
import org.scalatest.AsyncFlatSpec
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptSignature

class RpcClientTest extends AsyncFlatSpec {
  implicit val system = ActorSystem()
  implicit val m = ActorMaterializer()
  implicit val ec = m.executionContext
  implicit val regTestNetworkParam = RegTest

  val client = new RpcClient
  val logger = BitcoinSLogger.logger

  behavior of "RpcClient"

  it should "be able to get blockchain info" in {
    client.getBlockChainInfo.flatMap { info =>
      logger.info(info.toString)
      assert(info.chain == "regtest")
      client.getBestBlockHash.map( bestHash => assert(info.bestblockhash == bestHash))
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
              val input0 = TransactionOutPoint(transaction0.txid, UInt32(transaction0.blockindex.get))
              val input1 = TransactionOutPoint(transaction1.txid, UInt32(transaction1.blockindex.get))
              val sig: ScriptSignature = ScriptGenerators.scriptSignature.sample.get
              client.getNewAddress().flatMap { address =>
                client.createRawTransaction(Vector(TransactionInput(input0, sig, UInt32(0)), TransactionInput(input1, sig, UInt32(0))), Map((address.value, Bitcoins(1)))).map { info =>
                  logger.info(info.toString)
                  assert(true)
                }
              }
            }
          }
        }
      }
    }
  }

  it should "be able to get an address from bitcoind" in {
    val addressF = client.getNewAddress()
    addressF.map { address =>
      logger.info(address.value)
      assert(true)
    }
  }

  it should "be able to get an address from bitcoind given an account" in {
    val addressF = client.getNewAddress("")
    addressF.map { address =>
      logger.info(address.value)
      assert(true)
    }
  }

  it should "be able to get the block count" in {
    val blockCountF = client.getBlockCount
    blockCountF.map { count =>
      logger.info(count.toString)
      assert(true)
    }
  }

  it should "be able to get the connection count" in {
    val connectionCountF = client.getConnectionCount
    connectionCountF.map { count =>
      logger.info(count.toString)
      assert(count == 0)
    }
  }

  it should "be able to get the best block hash" in {
    val bestHashF = client.getBestBlockHash
    bestHashF.map { hash =>
      logger.info(hash.toString)
      assert(true)
    }
  }

  it should "be able to get the mining info" in {
    val miningInfoF = client.getMiningInfo
    miningInfoF.map { info =>
      logger.info(info.toString)
      assert(info.chain == "regtest")
    }
  }

  it should "be able to get the chain tips" in {
    val chainTipsF = client.getChainTips
    chainTipsF.map { tipArray =>
      tipArray.foreach { tip =>
        logger.info(tip.toString)
        assert(tip.status == "active")
      }
      assert(true)
    }
  }

  it should "be able to get the network info" in {
    val networkInfoF = client.getNetworkInfo
    networkInfoF.map { info =>
      logger.info(info.toString)
      assert(true)
    }
  }

  it should "be able to generate blocks" in {
    val blocksF = client.generate(3)
    blocksF.map { blocks =>
      blocks.foreach(block => logger.info(block.toString))
      assert(true)
    }
  }

  it should "be able to generate blocks and then get their serialized headers" in {
    val blocksF = client.generate(2)
    blocksF.flatMap { blocks =>
      blocks.foreach(block => logger.info(block.toString))
      val headerF1 = client.getBlockHeaderRaw(blocks(1))
      headerF1.map { header =>
        logger.info(header.toString)
        assert(header.previousBlockHashBE == blocks(0))
      }
    }
  }

  it should "be able to generate blocks and then get their headers" in {
    val blocksF = client.generate(2)
    blocksF.flatMap { blocks =>
      val headerF0 = client.getBlockHeader(blocks(0))
      headerF0.map { header =>
        logger.info(header.toString)
        assert(header.nextblockhash.contains(blocks(1)))
      }
      val headerF1 = client.getBlockHeader(blocks(1))
      headerF1.map { header =>
        logger.info(header.toString)
        assert(header.previousblockhash.contains(blocks(0)))
        assert(header.nextblockhash.isEmpty)
      }
    }
  }
}
