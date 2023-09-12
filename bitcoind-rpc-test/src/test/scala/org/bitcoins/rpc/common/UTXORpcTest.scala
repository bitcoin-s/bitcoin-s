package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import java.io.File
import java.nio.file.Files
import scala.concurrent.Future

class UTXORpcTest extends BitcoindRpcTest {

  lazy val clientF: Future[BitcoindRpcClient] =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(clientAccum = clientAccum)

  behavior of "UTXORpc"

  it should "be able to list utxos" in {
    for {
      client <- clientF
      unspent <- client.listUnspent
    } yield assert(unspent.nonEmpty)

  }

  it should "be able to lock and unlock utxos as well as list locked utxos" in {
    for {
      client <- clientF
      unspent <- client.listUnspent
      txid1 = unspent(0).txid
      txid2 = unspent(1).txid
      param = {

        val vout1 = unspent(0).vout
        val vout2 = unspent(1).vout
        Vector(RpcOpts.LockUnspentOutputParameter(txid1, vout1),
               RpcOpts.LockUnspentOutputParameter(txid2, vout2))
      }
      firstSuccess <- client.lockUnspent(unlock = false, param)
      locked <- client.listLockUnspent
      secondSuccess <- client.lockUnspent(unlock = true, param)
      newLocked <- client.listLockUnspent
    } yield {
      val txids = Vector(txid1, txid2)
      assert(firstSuccess)
      assert(locked.length == 2)
      assert(txids.exists(_ == locked(0).txId.flip))
      assert(txids.exists(_ == locked(1).txId.flip))
      assert(secondSuccess)
      assert(newLocked.isEmpty)
    }
  }

  it should "be able to get utxo info" in {
    for {
      client <- clientF
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      info1 <- client.getTxOut(block.tx.head.txid, 0)
    } yield assert(info1.coinbase)
  }

  it should "be able to fail to get utxo info" in {
    for {
      client <- clientF
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      info1 <- client.getTxOutOpt(block.tx.head.txid, 0)
      info2 <- client.getTxOutOpt(DoubleSha256DigestBE.empty, 0)
    } yield {
      assert(info1.exists(_.coinbase))
      assert(info2.isEmpty)
    }
  }

  it should "correctly dump tx out set" in {
    for {
      client <- clientF
      hash <- client.getBestBlockHash()
      height <- client.getBestHashBlockHeight()
      result <- client.dumpTxOutSet(new File("utxo.dat").toPath)
    } yield {
      assert(Files.exists(result.path))
      // Mild clean up
      Files.delete(result.path)

      assert(result.base_hash == hash)
      assert(result.base_height == height)
      assert(result.coins_written > 0)
    }
  }
}
