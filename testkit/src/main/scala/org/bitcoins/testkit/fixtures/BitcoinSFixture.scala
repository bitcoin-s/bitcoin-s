package org.bitcoins.testkit.fixtures

import akka.actor.ActorSystem
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.scalatest._

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

trait BitcoinSFixture extends fixture.AsyncFlatSpec with BitcoinSLogger {

  /**
    * Given functions to build and destroy a fixture, returns a OneArgAsyncTest => FutureOutcome
    * (this version gives the destroy function access to the fixture)
    *
    * Example:
    * {{{
    *   makeDependentFixture(createBitcoindChainHandler, destroyBitcoindChainHandler)
    * }}}
    */
  def makeDependentFixture[T](
      build: () => Future[T],
      destroy: T => Future[Any])(test: OneArgAsyncTest): FutureOutcome = {
    val fixtureF = build()

    val outcomeF = fixtureF.flatMap { fixture =>
      test(fixture.asInstanceOf[FixtureParam]).toFuture
    }

    val destroyP = Promise[Unit]()
    outcomeF.onComplete { _ =>
      fixtureF.foreach { fixture =>
        destroy(fixture).onComplete {
          case Success(_) => destroyP.success(())
          case Failure(err) =>
            logger.error(s"Failed to destroy fixture with err=${err}")
            destroyP.failure(err)
        }
      }
    }

    val outcomeAfterDestroyF = destroyP.future.flatMap(_ => outcomeF)

    new FutureOutcome(outcomeAfterDestroyF)
  }

  /**
    * Given functions to build and destroy a fixture, returns a OneArgAsyncTest => FutureOutcome
    * (this version does not give the destroy function access to the fixture, see makeDependentFixture)
    *
    * Example:
    * {{{
    *   makeFixture(createBlockHeaderDAO, destroyBlockHeaderTable)
    * }}}
    */
  def makeFixture[T](build: () => Future[T], destroy: () => Future[Any])(
      test: OneArgAsyncTest): FutureOutcome = {
    val outcomeF = build().flatMap { fixture =>
      test(fixture.asInstanceOf[FixtureParam]).toFuture
    }

    val destroyP = Promise[Unit]()
    outcomeF.onComplete { _ =>
      destroy().onComplete {
        case Success(_)   => destroyP.success(())
        case Failure(err) => destroyP.failure(err)
      }
    }

    val outcomeAfterDestroyF = destroyP.future.flatMap(_ => outcomeF)

    new FutureOutcome(outcomeAfterDestroyF)
  }

  /**
    * Given two fixture building methods (one dependent on the other), returns a single
    * fixture building method where the fixture is the pair of the two.
    *
    * Example:
    * {{{
    *   composeBuilders(createBlockHeaderDAO, createChainHandlerFromBlockHeaderDAO)
    * }}}
    */
  def composeBuilders[T, U](
      builder: () => Future[T],
      dependentBuilder: T => Future[U]): () => Future[(T, U)] = () => {
    builder().flatMap { first =>
      dependentBuilder(first).map { second =>
        (first, second)
      }
    }
  }

  /**
    * Given two fixture building methods (one dependent on the other) and a wrapper
    * for their pair type, returns a single fixture building method where the fixture is wrapper.
    *
    * Example:
    * {{{
    *   composeBuildersAndWrap(
    *       createBitcoind,
    *       createChainHandlerWithBitcoindZmq,
    *       BitcoindChainHandler.apply)
    * }}}
    */
  def composeBuildersAndWrap[T, U, C](
      builder: () => Future[T],
      dependentBuilder: T => Future[U],
      wrap: (T, U) => C): () => Future[C] = () => {
    composeBuilders(builder, dependentBuilder)().map {
      case (first, second) => wrap(first, second)
    }
  }

  /**
    *
    * Given two fixture building methods (one dependent on the other) and
    * a function that processes the result of the builders returning a Future,
    * returns a single fixture building method where the fixture is wrapper.
    *
    * This method is identical to `composeBuildersAndWrap`, except that
    * the wrapping function returns a `Future[C]` instead of a `C`
    */
  def composeBuildersAndWrapFuture[T, U, C](
      builder: () => Future[T],
      dependentBuilder: T => Future[U],
      processResult: (T, U) => Future[C]
  ): () => Future[C] = () => {
    composeBuilders(builder, dependentBuilder)().flatMap {
      case (first, second) => processResult(first, second)
    }
  }

}

object BitcoinSFixture {

  def createBitcoindWithFunds()(
      implicit system: ActorSystem): Future[BitcoindRpcClient] = {
    import system.dispatcher
    for {
      bitcoind <- createBitcoind()
      address <- bitcoind.getNewAddress
      _ <- bitcoind.generateToAddress(blocks = 101, address)
    } yield bitcoind
  }

  /** Creates a new bitcoind instance */
  def createBitcoind()(
      implicit system: ActorSystem): Future[BitcoindRpcClient] = {
    import system.dispatcher
    val instance = BitcoindRpcTestUtil.instance()
    val bitcoind = new BitcoindRpcClient(instance)

    bitcoind.start().map(_ => bitcoind)
  }
}
