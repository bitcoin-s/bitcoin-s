package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesFundedCachedNewest,
  BitcoindRpcTestUtil
}

import java.io.File
import java.nio.file.Files

class UTXORpcTest extends BitcoindFixturesFundedCachedNewest {

  behavior of "UTXORpc"

  it should "be able to list utxos" in { case client =>
    for {
      unspent <- client.listUnspent
    } yield assert(unspent.nonEmpty)

  }

  it should "be able to lock and unlock utxos as well as list locked utxos" in {
    case client =>
      for {
        _ <- client.generate(1)
        unspent <- client.listUnspent
        txid1 = unspent(0).txid
        txid2 = unspent(1).txid
        param = {

          val vout1 = unspent(0).vout
          val vout2 = unspent(1).vout
          Vector(
            RpcOpts.LockUnspentOutputParameter(txid1, vout1),
            RpcOpts.LockUnspentOutputParameter(txid2, vout2)
          )
        }
        firstSuccess <- client.lockUnspent(unlock = false, param)
        locked <- client.listLockUnspent
        secondSuccess <- client.lockUnspent(unlock = true, param)
        newLocked <- client.listLockUnspent
      } yield {
        val txids = Vector(txid1, txid2)
        assert(firstSuccess)
        assert(locked.length == 2)
        assert(txids.exists(_ == locked(0).txIdBE))
        assert(txids.exists(_ == locked(1).txIdBE))
        assert(secondSuccess)
        assert(newLocked.isEmpty)
      }
  }

  it should "be able to get utxo info" in { case client =>
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      info1 <- client.getTxOut(block.tx.head.txid, 0)
    } yield assert(info1.coinbase)
  }

  it should "be able to fail to get utxo info" in { case client =>
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      info1 <- client.getTxOutOpt(block.tx.head.txid, 0)
      info2 <- client.getTxOutOpt(DoubleSha256DigestBE.empty, 0)
    } yield {
      assert(info1.exists(_.coinbase))
      assert(info2.isEmpty)
    }
  }

  it should "correctly dump tx out set and then load it" in { case client =>
    val path = new File("utxo.dat").toPath
    for {
      hash <- client.getBestBlockHash()
      height <- client.getBestHashBlockHeight()
      result <- client.dumpTxOutSet(path)
      // now attempt to load it
      //unfortunately it seems this cannot be properly tested
      //we end up with this error:
      //Unable to load UTXO snapshot, assumeutxo block hash in snapshot metadata not recognized
      //see: https://bitcoin.stackexchange.com/questions/121006/anyone-tried-assumeutxo-yet
      //loadResult <- client.loadTxOutSet(path)
    } yield {
      assert(Files.exists(result.path))

      assert(result.base_hash == hash)
      assert(result.base_height == height)
      assert(result.coins_written > 0)

      //assert(loadResult.path == path)
      //assert(loadResult.tip_hash == hash)
      //assert(loadResult.base_height == height)
      //assert(loadResult.coins_loaded > 0)

      // Mild clean up
      Files.delete(result.path)
      succeed
    }
  }
}
