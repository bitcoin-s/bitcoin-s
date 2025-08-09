package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.{RpcOpts, ScanTxoutSetRequest}
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{
  DumpTxOutSetType,
  ScanBlocksOpt
}
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits}
import org.bitcoins.core.protocol.script.descriptor.AddressDescriptor
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
      unspent0 <- client.listUnspent
      address <- client.getNewAddress
      _ <- client.sendToAddress(address, Bitcoins.one)
      _ <- client.generate(1)
      unspent1 <- client.listUnspent(Vector(address))
    } yield {
      assert(unspent0.nonEmpty)
      assert(unspent1.size == 1)
    }

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
    } yield {
      assert(info1.isDefined)
      assert(info1.get.coinbase)
    }
  }

  it should "be able to fail to get utxo info" in { case client =>
    for {
      info2 <- client.getTxOut(DoubleSha256DigestBE.empty, 0)
    } yield {
      assert(info2.isEmpty)
    }
  }

  it should "correctly dump tx out set and then load it" in { case client =>
    val path = new File("utxo.dat").toPath
    for {
      hash <- client.getBestBlockHash()
      height <- client.getBestHashBlockHeight()
      result <- client.dumpTxOutSet(path, DumpTxOutSetType.Latest)
      // now attempt to load it
      // unfortunately it seems this cannot be properly tested
      // we end up with this error:
      // Unable to load UTXO snapshot, assumeutxo block hash in snapshot metadata not recognized
      // see: https://bitcoin.stackexchange.com/questions/121006/anyone-tried-assumeutxo-yet
      // loadResult <- client.loadTxOutSet(path)
    } yield {
      assert(Files.exists(result.path))

      assert(result.base_hash == hash)
      assert(result.base_height == height)
      assert(result.coins_written > 0)

      // assert(loadResult.path == path)
      // assert(loadResult.tip_hash == hash)
      // assert(loadResult.base_height == height)
      // assert(loadResult.coins_loaded > 0)

      // Mild clean up
      Files.delete(result.path)
      succeed
    }
  }

  it should "scantxoutset to find a utxo" in { client =>
    val numBlocks = 6
    val amt = Bitcoins.one
    for {
      address <- client.getNewAddress
      txid <- client.sendToAddress(address, amt)
      descs = Vector(AddressDescriptor(address))
      request0 =
        ScanTxoutSetRequest(action = ScanBlocksOpt.Start, descs)
      _ <- client.generate(numBlocks)
      response0 <- client.scanTxoutSet(request0)
      balance <- client.getBalance
      sweepAddr <- client.getNewAddress
      _ <- client.sendToAddress(sweepAddr, balance, subractFeeFromAmount = true)
      blockCount <- client.getBlockCount()
      _ <- client.generate(numBlocks)
      response1 <- client.scanTxoutSet(request0)
    } yield {
      assert(response0.success)
      assert(response0.height == blockCount)
      assert(response0.unspents.size == 1)
      response0.unspents.foreach { u =>
        assert(u.txid == txid)
        assert(u.height == blockCount - numBlocks + 1)
        assert(u.confirmations == numBlocks)
        assert(u.amount == amt)
        assert(u.scriptPubKey == address.scriptPubKey)
      }
      assert(response0.total_amount == amt)

      assert(response1.unspents.isEmpty)
      assert(response1.total_amount == CurrencyUnits.zero)
    }

  }
}
