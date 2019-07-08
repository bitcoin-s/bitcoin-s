package org.bitcoins.testkit.util

import scala.io.Source
import play.api.libs.json._

object ScriptTestUtil {

  /** Script test cases from `script_tests.json` */
  lazy val testCases = {
    val source = Source.fromURL(getClass.getResource("/script_tests.json"))
    val lines =
      try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n"
      finally source.close()
    val json = Json.parse(lines)
    // val testCasesOpt: Seq[Option[CoreTestCase]] =
    //  json.convertTo[Seq[Option[CoreTestCase]]]
    // val testCases: Seq[CoreTestCase] = testCasesOpt.flatten
    // testCases
    ???

  }
}
