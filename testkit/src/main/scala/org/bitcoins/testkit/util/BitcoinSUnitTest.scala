package org.bitcoins.testkit.util

import org.scalactic.anyvals.PosInt
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}
import org.slf4j.{Logger, LoggerFactory}

/** A wrapper for boiler plate testing procesures in bitcoin-s */
abstract class BitcoinSUnitTest
    extends FlatSpec
    with MustMatchers
    with ScalaCheckPropertyChecks {

  protected lazy val logger: Logger = LoggerFactory.getLogger(getClass)

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
      workers = 2
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

private object BitcoinSUnitTest {

  /** The number of times new code
    * should be executed in a property based test
    */
  val NEW_CODE_EXECUTIONS = 100

  /** The number of times old code should be executed
    * in a property based test
    */
  val OLD_CODE_EXECUTIONS = 20

}
