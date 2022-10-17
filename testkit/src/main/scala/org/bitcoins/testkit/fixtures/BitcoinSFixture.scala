package org.bitcoins.testkit.fixtures

import akka.actor.ActorSystem
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v19.V19BlockFilterRpc
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoinSAsyncFixtureTest
import org.scalatest._

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

trait BitcoinSFixture extends BitcoinSAsyncFixtureTest {

  /** Given functions to build and destroy a fixture, returns a OneArgAsyncTest => FutureOutcome
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
    val fixtureF: Future[T] = build()

    val outcomeF: Future[Outcome] = fixtureF
      .flatMap { fixture =>
        test(fixture.asInstanceOf[FixtureParam]).toFuture
      }
      .recoverWith { case err =>
        FutureOutcome.failed(err).toFuture
      }

    val destructedF: Future[Outcome] = outcomeF.transformWith {
      case Success(o) =>
        for {
          t <- fixtureF
          _ <- destroy(t)
        } yield o
      case Failure(exn) =>
        for {
          t <- fixtureF
          _ <- destroy(t)
        } yield {
          throw exn
        }
    }
    new FutureOutcome(destructedF)
  }

  /** Given functions to build and destroy a fixture, returns a OneArgAsyncTest => FutureOutcome
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

  override def afterAll(): Unit = {
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

object BitcoinSFixture {

  def createBitcoindWithFunds(versionOpt: Option[BitcoindVersion] = None)(
      implicit system: ActorSystem): Future[BitcoindRpcClient] = {
    import system.dispatcher
    for {
      bitcoind <- createBitcoind(versionOpt = versionOpt)
      address <- bitcoind.getNewAddress
      _ <- bitcoind.generateToAddress(blocks = 101, address)
    } yield bitcoind
  }

  def createBitcoindBlockFilterRpcWithFunds(
      versionOpt: Option[BitcoindVersion] = None)(implicit
      system: ActorSystem): Future[BitcoindRpcClient with V19BlockFilterRpc] = {
    import system.dispatcher
    for {
      bitcoind <- createBitcoindWithFunds(versionOpt)
      _ = require(
        bitcoind.isInstanceOf[V19BlockFilterRpc],
        s"Given version does not support block filter rpc, got=$versionOpt")
    } yield bitcoind.asInstanceOf[BitcoindRpcClient with V19BlockFilterRpc]
  }

  /** Creates a new bitcoind instance
    * @param versionOpt the version of bitcoind ot use
    * @param enableNeutrinoOpt whether neutrino should be enabled or not, if param not given it is default enabled
    */
  def createBitcoind(
      versionOpt: Option[BitcoindVersion] = None,
      enableNeutrino: Boolean = true)(implicit
      system: ActorSystem): Future[BitcoindRpcClient] = {
    import system.dispatcher
    val instance = BitcoindRpcTestUtil.instance(versionOpt = versionOpt,
                                                enableNeutrino = enableNeutrino)
    val bitcoind = versionOpt match {
      case Some(v) => BitcoindRpcClient.fromVersion(v, instance)
      case None    => new BitcoindRpcClient(instance)
    }
    BitcoindRpcTestUtil.startServers(Vector(bitcoind)).map(_ => bitcoind)
  }

  /** Given two fixture building methods (one dependent on the other), returns a single
    * fixture building method where the fixture is the pair of the two.
    *
    * Example:
    * {{{
    *   composeBuilders(createBlockHeaderDAO, createChainHandlerFromBlockHeaderDAO)
    * }}}
    */
  def composeBuilders[T, U](
      builder: () => Future[T],
      dependentBuilder: T => Future[U])(implicit
      ec: ExecutionContext): () => Future[(T, U)] =
    () => {
      builder().flatMap { first =>
        dependentBuilder(first).map { second =>
          (first, second)
        }
      }
    }

  /** Given two fixture building methods (one dependent on the other) and a wrapper
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
      wrap: (T, U) => C)(implicit ec: ExecutionContext): () => Future[C] =
    () => {
      composeBuilders(builder, dependentBuilder)(ec)().map {
        case (first, second) =>
          wrap(first, second)
      }
    }

  /** Given two fixture building methods (one dependent on the other) and
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
  )(implicit ec: ExecutionContext): () => Future[C] =
    () => {
      composeBuilders(builder, dependentBuilder)(ec)().flatMap {
        case (first, second) => processResult(first, second)
      }
    }

}
