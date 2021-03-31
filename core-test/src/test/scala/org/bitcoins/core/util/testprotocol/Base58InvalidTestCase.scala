package org.bitcoins.core.util.testprotocol

import ujson._
import upickle.default._

case class Base58InvalidTestCase(base58EncodedString: String)

object Base58InvalidTestCase {

  implicit val base58InvalidTestCaseR: Reader[Base58InvalidTestCase] =
    reader[Value].map {
      case array: Arr =>
        val str = array.value.head.str
        Base58InvalidTestCase(str)
      case error: Value =>
        throw new RuntimeException("Expected array. Got: " + error)
    }
}
