package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.protocol.script.{ScriptSignature, TaprootKeyPath}
import org.bitcoins.core.script.flag.ScriptVerifyTaproot
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.testkitcore.util.BitcoinSJvmTest
import org.scalatest.Assertion
import org.scalatest.time.Span
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class TaprootTxTests extends BitcoinSJvmTest {

  behavior of "Taproot test cases"

  //these static test vectors take forever
  override lazy val timeLimit: Span = 10.minutes

  //these tests are from
  //https://raw.githubusercontent.com/bitcoin-core/qa-assets/main/unit_test_data/script_assets_test.json
  lazy val url = getClass.getResource("/script_assets_test_cp.json")

  lazy val lines = {
    scala.io.Source.fromURL(url).getLines().mkString
  }

  lazy val testCases: Seq[TaprootTestCase] = {
    upickle.default.read[Seq[TaprootTestCase]](lines)
  }
  it must "parse a taproot test case" in {
    //https://github.com/bitcoin/bitcoin/blob/v22.0/test/functional/feature_taproot.py#L1112
    //https://github.com/bitcoin/bitcoin/blob/3820090bd619ac85ab35eff376c03136fe4a9f04/src/test/script_tests.cpp#L1673
    val first = testCases.head
    val expectedTxHex =
      "f705d6e8019870958e85d1d8f94aa6d74746ba974db0f5ccae49a49b32dcada4e19de4eb5ecb00000000925977cc01f9875c000000000016001431d2b00cd4687ceb34008d9894de84062def14aa05406346"
    val expectedPrevOutHex =
      "b4eae1010000000022512039f7e9232896f8100485e38afa652044f855e734a13b840a3f220cbd5d911ad5"
    assert(first.flags.exists(_ == ScriptVerifyTaproot))
    assert(first.tx.hex == expectedTxHex)
    assert(first.prevouts.map(_.hex) == Vector(expectedPrevOutHex))
    assert(first.index == 0)
    assert(first.success._1 == ScriptSignature.empty)

    val witHex =
      "25e45bd4d4b8fcd5933861355a2d376aad8daf1af1588e5fb6dfcea22d0d809acda6fadca11e97f5b5c85af99df27cb24fa69b08fa6c790234cdc671d3af5a7302"
    val witBytes = ByteVector.fromValidHex(witHex)
    val expectedWitness = TaprootKeyPath.fromStack(Vector(witBytes))
    assert(first.success._2.get == expectedWitness)
    assert(first.`final` == Some(true))
  }

  it must "run the success test cases through the script interpreter" in {
    //execute in parallel as running test cases sequentially takes 17 minutes on CI
    val groupedTestCases =
      testCases.grouped(Runtime.getRuntime.availableProcessors())

    val execute: Vector[Future[Vector[Assertion]]] = {
      groupedTestCases
        .map(cases => executeSuccessTestCases(cases.toVector))
        .toVector
    }
    Future
      .sequence(execute)
      .map(_.flatten)
      .map(_ => succeed)
  }

  private def executeSuccessTestCases(
      testCases: Vector[TaprootTestCase]): Future[Vector[Assertion]] = {
    Future {
      testCases.map { testCase =>
        withClue(testCase.comment) {
          val result = ScriptInterpreter.run(testCase.successProgram)
          assert(result == ScriptOk)
        }
      }
    }
  }

  it must "run the failure test cases through the script interpreter" ignore {
    testCases.foreach { testCase =>
      testCase.failureTxSigComponentsOpt match {
        case Some(_) =>
          withClue(testCase.comment) {
            val result = ScriptInterpreter.run(testCase.failProgramOpt.get)
            assert(result != ScriptOk)
          }
        case None =>
          ()
      }
    }

    succeed
  }
}
