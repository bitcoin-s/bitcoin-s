package org.bitcoins.testkitcore.util

import org.scalacheck.Shrink
import org.scalactic.anyvals.PosInt
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.time.Span
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration.DurationInt

/** A wrapper for boi ler plate testing procesures in bitcoin-s */
abstract class BitcoinSUnitTest
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with TimeLimitedTests {

  override val timeLimit: Span = 1000.seconds

  /** This def ensures that shrinks are disabled for all calls to forAll.
    *
    * If you want to enable shrinking for a specific test, introduce an
    * implicit val into that scope with type Shrink[T] where T is the type
    * of the generator you want to enable shrinking on.
    */
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny[T]

  /** The configuration for property based tests in our testing suite
    * @see http://www.scalatest.org/user_guide/writing_scalacheck_style_properties
    */
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = {
    generatorDriveConfigOldCode
  }

  /** Sets the generator driven tests to perform the given amount of execs */
  def customGenDrivenConfig(executions: Int): PropertyCheckConfiguration = {
    PropertyCheckConfiguration(
      minSuccessful = PosInt.from(executions).get,
      minSize = PosInt.from(executions).get,
      workers = 1
    )
  }

  /** Property based tests that have been around a long time
    * have a less of a chance failing, so execute them less
    * @return
    */
  def generatorDriveConfigOldCode: PropertyCheckConfiguration = {
    customGenDrivenConfig(BitcoinSUnitTest.OLD_CODE_EXECUTIONS)
  }

  /** Property based tests that are new have a higher chance of failing
    * so execute them more
    * @return
    */
  def generatorDrivenConfigNewCode: PropertyCheckConfiguration = {
    customGenDrivenConfig(BitcoinSUnitTest.NEW_CODE_EXECUTIONS)
  }

}

object BitcoinSUnitTest {

  /** The number of times new code
    * should be executed in a property based test
    */
  val NEW_CODE_EXECUTIONS = 100

  /** The number of times old code should be executed
    * in a property based test
    */
  val OLD_CODE_EXECUTIONS = 20

}
