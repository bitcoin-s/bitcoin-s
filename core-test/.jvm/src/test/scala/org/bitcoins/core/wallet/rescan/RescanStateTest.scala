package org.bitcoins.core.wallet.rescan

import org.bitcoins.core.wallet.rescan.RescanState.{
  RescanStarted,
  RescanTerminatedEarly
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import scala.concurrent.Promise

class RescanStateTest extends BitcoinSAsyncTest {

  behavior of "RescanState"

  it must "stop a rescan that has started" in {
    val completeRescanEarlyP = Promise[Option[Int]]()
    val recursiveRescanP = Promise[RescanState]()
    val blocksMatchedF = completeRescanEarlyP.future.map(_ => Vector.empty)
    val rescanState = RescanState.RescanStarted(completeRescanEarlyP,
                                                blocksMatchedF,
                                                recursiveRescanP)
    for {
      blockMatches <- rescanState.stop()
    } yield assert(blockMatches.isEmpty)
  }

  it must "track a single rescan correctly" in {
    val completeRescanEarlyP = Promise[Option[Int]]()
    val recursiveRescanP = Promise[RescanState]()
    val blocksMatchedF = completeRescanEarlyP.future.map(_ => Vector.empty)
    val rescanState = RescanState.RescanStarted(completeRescanEarlyP,
                                                blocksMatchedF,
                                                recursiveRescanP)
    val _ = completeRescanEarlyP.success(None)
    for {
      _ <- rescanState.singleRescanDoneF
      _ = assert(!rescanState.entireRescanDoneF.isCompleted)
      _ = recursiveRescanP.success(RescanState.RescanNotNeeded)
      _ <- rescanState.entireRescanDoneF
    } yield succeed
  }

  it must "propagate an exception if its not RescanTerminatedEarly" in {
    val completeRescanEarlyP = Promise[Option[Int]]()
    val completeRescanEarly1P = Promise[Option[Int]]()
    val recursiveRescanP = Promise[RescanState]()
    val recursiveRescan1P = Promise[RescanState]()
    val blocksMatchedF = completeRescanEarlyP.future.map(_ => Vector.empty)
    val blocksMatched1F = completeRescanEarly1P.future.map(_ => Vector.empty)

    val rescanState =
      RescanState.RescanStarted(completeRescanEarlyP = completeRescanEarlyP,
                                blocksMatchedF = blocksMatchedF,
                                recursiveRescanP = recursiveRescanP)
    val rescanState1 =
      RescanStarted(completeRescanEarly1P, blocksMatched1F, recursiveRescan1P)
    val _ = rescanState.fail(RescanTerminatedEarly)

    val resultF = for {
      vec0 <- rescanState.entireRescanDoneF
      _ = assert(vec0.isEmpty)
      _ = rescanState1.fail(
        new RuntimeException("Should fail with generic exception"))
      _ <- rescanState1.stop()
    } yield {
      succeed
    }

    recoverToSucceededIf[RuntimeException](resultF)
  }

  it must "handle stopping recursive rescan correctly" in {
    val completeRescanEarlyP = Promise[Option[Int]]()
    val recursiveRescanP = Promise[RescanState]()
    val blocksMatchedF = completeRescanEarlyP.future.map(_ => Vector.empty)
    val rescanState = RescanState.RescanStarted(completeRescanEarlyP,
                                                blocksMatchedF,
                                                recursiveRescanP)

    val recursiveRescanCompleteEarlyP = Promise[Option[Int]]()
    val recursiveRescanStarted = RescanState.RescanStarted(
      recursiveRescanCompleteEarlyP,
      recursiveRescanCompleteEarlyP.future.map(_ => Vector.empty),
      Promise())
    recursiveRescanP.success(recursiveRescanStarted)
    for {
      _ <- rescanState.stop()
      _ <- recoverToSucceededIf[RescanTerminatedEarly.type](
        rescanState.blocksMatchedF)
    } yield succeed
  }
}
