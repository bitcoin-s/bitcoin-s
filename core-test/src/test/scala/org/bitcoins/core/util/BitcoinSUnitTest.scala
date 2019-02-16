package org.bitcoins.core.util

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}
import org.slf4j.{Logger, LoggerFactory}

/** A wrapper for boiler plate testing procesures in bitcoin-s */
abstract class BitcoinSUnitTest
    extends FlatSpec
    with MustMatchers
    with PropertyChecks {

  lazy protected val logger: Logger = LoggerFactory.getLogger(getClass)

  /** The configuration for property based tests in our testing suite
    * See: http://www.scalatest.org/user_guide/writing_scalacheck_style_properties
    */
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = {
    generatorDriveConfigOldCode
  }

  private def buildConfig(executions: Int): PropertyCheckConfiguration = {
    PropertyCheckConfig(
      minSuccessful = executions,
      minSize = executions,
      maxSize = executions
    )
  }

  /** Property based tests that have been around a long time
    * have a less of a chance failing, so execute them less
    * @return
    */
  def generatorDriveConfigOldCode: PropertyCheckConfiguration = {
    buildConfig(BitcoinSUnitTest.OLD_CODE_EXECUTIONS)
  }

  /** Property based tests that are new have a higher chance of failing
    * so execute them more
    * @return
    */
  def generatorDrivenConfigNewCode: PropertyCheckConfiguration = {
    buildConfig(BitcoinSUnitTest.NEW_CODE_EXECUTIONS)
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
