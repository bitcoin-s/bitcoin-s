package org.bitcoins.core.serializers

import org.bitcoins.core.util.{BitcoinSLogger, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json.{JsObject, JsValue, JsString}

/**
 * Created by chris on 3/30/16.
 */
class AddressProtocolTest extends FlatSpec with MustMatchers with BitcoinSLogger {

  it must "write a bitcoin address" in {
    AddressProtocol.AddressFormat.write(TestUtil.bitcoinAddress).fields("address") must be (JsString(TestUtil.bitcoinAddress.value))
  }


}
