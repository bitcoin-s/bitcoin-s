package org.bitcoins.rpc

import akka.actor.ActorSystem
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.BitcoindRpcClient

import scala.concurrent.duration.{ DurationInt, FiniteDuration }
import scala.concurrent.{ Await, Future, Promise }

trait RpcUtil extends BitcoinSLogger {

  private def retryRunnable(condition: => Boolean, p: Promise[Boolean]): Runnable = new Runnable {
    override def run(): Unit = {
      p.success(condition)
      ()
    }
  }

  /**
   * The returned Future completes when condition becomes true
   * @param condition The condition being waited on
   * @param duration The interval between calls to check condition
   * @param maxTries If condition is tried this many times, the Future fails
   * @param system An ActorSystem to schedule calls to condition
   * @return A Future[Unit] that succeeds if condition becomes true and fails otherwise
   */
  def retryUntilSatisfied(
    condition: => Boolean,
    duration: FiniteDuration = 100.milliseconds,
    maxTries: Int = 50)(implicit system: ActorSystem): Future[Unit] = {
    retryUntilSatisfiedWithCounter(condition, duration, maxTries = maxTries)
  }

  // Has a different name so that default values are permitted
  private def retryUntilSatisfiedWithCounter(
    condition: => Boolean,
    duration: FiniteDuration,
    counter: Int = 0,
    maxTries: Int)(implicit system: ActorSystem): Future[Unit] = {
    implicit val ec = system.dispatcher
    if (counter == maxTries) {
      Future.failed(new RuntimeException("Condition timed out"))
    } else if (condition) {
      Future.successful(())
    } else {

      val p = Promise[Boolean]()
      val runnable = retryRunnable(condition, p)

      system.scheduler.scheduleOnce(duration, runnable)

      p.future.flatMap {
        case true => Future.successful(())
        case false => retryUntilSatisfiedWithCounter(condition, duration, counter + 1, maxTries)
      }

    }
  }

  /**
   * Blocks until condition becomes true, the condition
   * is checked maxTries times, or overallTimeout is reached
   * @param condition The blocking condition
   * @param duration The interval between calls to check condition
   * @param maxTries If condition is tried this many times, an exception is thrown
   * @param overallTimeout If this much time passes, an exception is thrown.
   *                       This exists in case calls to condition take significant time,
   *                       otherwise just use duration and maxTries to configure timeout.
   * @param system An ActorSystem to schedule calls to condition
   */
  def awaitCondition(
    condition: => Boolean,
    duration: FiniteDuration = 100.milliseconds,
    maxTries: Int = 50,
    overallTimeout: FiniteDuration = 1.hour)(implicit system: ActorSystem): Unit = {
    Await.result(retryUntilSatisfied(condition, duration, maxTries), overallTimeout)
  }

  def awaitServer(
    server: BitcoindRpcClient,
    duration: FiniteDuration = 100.milliseconds,
    maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    awaitCondition(server.isStarted, duration, maxTries)
  }

  def awaitServerShutdown(
    server: BitcoindRpcClient,
    duration: FiniteDuration = 300.milliseconds,
    maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    awaitCondition(!server.isStarted, duration, maxTries)
  }
}

object RpcUtil extends RpcUtil
