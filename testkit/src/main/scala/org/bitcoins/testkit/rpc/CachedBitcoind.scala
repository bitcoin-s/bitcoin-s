package org.bitcoins.testkit.rpc

import org.bitcoins.rpc.client.common.BitcoindRpcClient
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
trait CachedBitcoind { _: BitcoinSAkkaAsyncTest =>

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
  private[this] val isBitcoindUsed: AtomicBoolean = new AtomicBoolean(false)

  /** The bitcoind instance, lazyily created */
  protected lazy val cachedBitcoindWithFundsF: Future[BitcoindRpcClient] = {
    isBitcoindUsed.set(true)
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
