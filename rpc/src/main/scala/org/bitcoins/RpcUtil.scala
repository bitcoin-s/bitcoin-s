package org.bitcoins

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

  def retryUntilSatisfied(
    condition: => Boolean,
    duration: FiniteDuration = 100.milliseconds,
    maxTries: Int = 50)(implicit system: ActorSystem): Future[Unit] = {
    val f = () => Future.successful(condition)
    retryUntilSatisfiedF(f, duration, maxTries)
  }

  /**
   * The returned Future completes when condition becomes true
   * @param conditionF The condition being waited on
   * @param duration The interval between calls to check condition
   * @param maxTries If condition is tried this many times, the Future fails
   * @param system An ActorSystem to schedule calls to condition
   * @return A Future[Unit] that succeeds if condition becomes true and fails otherwise
   */
  def retryUntilSatisfiedF(
    conditionF: () => Future[Boolean],
    duration: FiniteDuration = 100.milliseconds,
    maxTries: Int = 50)(implicit system: ActorSystem): Future[Unit] = {

    retryUntilSatisfiedWithCounter(
      conditionF = conditionF,
      duration = duration,
      maxTries = maxTries)
  }

  // Has a different name so that default values are permitted
  private def retryUntilSatisfiedWithCounter(
    conditionF: () => Future[Boolean],
    duration: FiniteDuration,
    counter: Int = 0,
    maxTries: Int)(implicit system: ActorSystem): Future[Unit] = {

    implicit val ec = system.dispatcher

    conditionF().flatMap { condition =>

      if (condition) {
        Future.successful(())
      } else if (counter == maxTries) {
        Future.failed(new RuntimeException("Condition timed out"))
      } else {

        val p = Promise[Boolean]()
        val runnable = retryRunnable(condition, p)

        system.scheduler.scheduleOnce(duration, runnable)

        p.future.flatMap {
          case true => Future.successful(())
          case false => retryUntilSatisfiedWithCounter(conditionF, duration, counter + 1, maxTries)
        }
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
    condition: () => Boolean,
    duration: FiniteDuration = 100.milliseconds,
    maxTries: Int = 50,
    overallTimeout: FiniteDuration = 1.hour)(implicit system: ActorSystem): Unit = {

    //type hackery here to go from () => Boolean to () => Future[Boolean]
    //to make sure we re-evaluate every time retryUntilSatisfied is called
    def conditionDef: Boolean = condition()
    val conditionF: () => Future[Boolean] = () => Future.successful(conditionDef)

    awaitConditionF(conditionF, duration, maxTries, overallTimeout)
  }

  def awaitConditionF(
    conditionF: () => Future[Boolean],
    duration: FiniteDuration = 100.milliseconds,
    maxTries: Int = 50,
    overallTimeout: FiniteDuration = 1.hour)(implicit system: ActorSystem): Unit = {

    val f: Future[Unit] = retryUntilSatisfiedF(
      conditionF = conditionF,
      duration = duration,
      maxTries = maxTries)

    Await.result(f, overallTimeout)
  }

  def awaitServer(
    server: BitcoindRpcClient,
    duration: FiniteDuration = 1.seconds,
    maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    val f = () => server.isStarted
    awaitCondition(f, duration, maxTries)
  }

  def awaitServerShutdown(
    server: BitcoindRpcClient,
    duration: FiniteDuration = 300.milliseconds,
    maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    val f = () => !server.isStarted
    awaitCondition(f, duration, maxTries)
  }
}

object RpcUtil extends RpcUtil
