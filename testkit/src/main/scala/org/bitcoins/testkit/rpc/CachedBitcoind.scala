package org.bitcoins.testkit.rpc

import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.rpc.util.NodeTriple
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.BitcoinSAkkaAsyncTest

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{Await, Future}

/** A trait that holds a cached instance of a [[org.bitcoins.rpc.client.common.BitcoindRpcClient]]
  * This is useful for using with fixtures to avoid creating a new bitcoind everytime a
  * new test is run.
  *
  * The idea is our wallet/chain/node can just use the cached bitcoind rather than a fresh one.
  * This does mean that test cases have to be written in such a way where assertions
  * are not dependent on specific bitcoind state.
  */
trait CachedBitcoind { _: BitcoinSAkkaAsyncTest => }

trait CachedBitcoindFunded extends CachedBitcoind { _: BitcoinSAkkaAsyncTest =>

  /** Flag to indicate if the bitcoind was used
    *
    * If we don't have this, we have no way
    * to know if we should cleanup the cached bitcoind
    * in the [[afterAll()]] method or not.
    *
    * We do not want to accidentally create a bitcoind
    * inside of [[afterAll()]] just to have it
    * cleaned up in the same method.
    */
  protected val isBitcoindUsed: AtomicBoolean = new AtomicBoolean(false)

  /** The bitcoind instance, lazyily created */
  protected lazy val cachedBitcoindWithFundsF: Future[BitcoindRpcClient] = {
    val _ = isBitcoindUsed.set(true)
    BitcoinSFixture
      .createBitcoindWithFunds(None)
  }

  override def afterAll(): Unit = {
    if (isBitcoindUsed.get()) {
      //if it was used, shut down the cached bitcoind
      val stoppedF = for {
        cachedBitcoind <- cachedBitcoindWithFundsF
        _ <- BitcoindRpcTestUtil.stopServer(cachedBitcoind)
      } yield ()

      Await.result(stoppedF, duration)
    } else {
      //do nothing since bitcoind wasn't used
    }
  }
}

trait CachedBitcoindNewest extends CachedBitcoindFunded {
  _: BitcoinSAkkaAsyncTest =>

  override protected lazy val cachedBitcoindWithFundsF: Future[
    BitcoindRpcClient] = {
    val _ = isBitcoindUsed.set(true)
    BitcoinSFixture
      .createBitcoindWithFunds(Some(BitcoindVersion.newest))
  }
}

trait CachedBitcoindV19 extends CachedBitcoindFunded {
  _: BitcoinSAkkaAsyncTest =>

  override protected lazy val cachedBitcoindWithFundsF: Future[
    BitcoindV19RpcClient] = {
    val _ = isBitcoindUsed.set(true)
    BitcoinSFixture
      .createBitcoindWithFunds(Some(BitcoindVersion.V19))
      .map(_.asInstanceOf[BitcoindV19RpcClient])
  }
}

trait CachedBitcoindTriple extends CachedBitcoind { _: BitcoinSAkkaAsyncTest =>

  /** Flag to indicate if the bitcoinds were used
    *
    * If we don't have this, we have no way
    * to know if we should cleanup the cached bitcoind
    * in the [[afterAll()]] method or not.
    *
    * We do not want to accidentally create a bitcoind
    * inside of [[afterAll()]] just to have it
    * cleaned up in the same method.
    */
  private val isClientsUsed: AtomicBoolean = new AtomicBoolean(false)

  lazy val clientsF: Future[NodeTriple] = {
    val _ = isClientsUsed.set(true)
    BitcoindRpcTestUtil
      .createNodeTriple()
      .map(NodeTriple.fromTuple(_))
  }

  override def afterAll(): Unit = {
    if (isClientsUsed.get()) {
      //if it was used, shut down the cached bitcoind
      val stoppedF = for {
        clients <- clientsF
        _ <- BitcoindRpcTestUtil.stopServers(clients.toVector)
      } yield ()

      Await.result(stoppedF, duration)
    } else {
      //do nothing since bitcoind wasn't used
    }
  }

}
