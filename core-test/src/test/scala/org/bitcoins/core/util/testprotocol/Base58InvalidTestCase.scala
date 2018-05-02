package org.bitcoins.core.util.testprotocol

/**
 * Created by tom on 6/21/16.
 */

trait Base58InvalidTestCase {
  def base58EncodedString: String
}

case class Base58InvalidTestCaseImpl(base58EncodedString: String) extends Base58InvalidTestCase

