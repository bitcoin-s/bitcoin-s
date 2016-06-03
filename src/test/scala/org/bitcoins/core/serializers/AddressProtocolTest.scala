package org.bitcoins.core.serializers

import org.bitcoins.core.util.{BitcoinSLogger, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json.{JsObject, JsValue, JsString}

/**
 * Created by chris on 3/30/16.
 */
class AddressProtocolTest extends FlatSpec with MustMatchers with BitcoinSLogger {

  "BitcoinAddressProtocol" must "read a bitcoin address from a json string" in {
    val m : Map[String,JsValue] = Map("value" -> JsString(TestUtil.bitcoinAddress.value))
    val obj = JsObject(m)
    BitcoinAddressProtocol.bitcoinAddressFormat.read(obj) must be (TestUtil.bitcoinAddress)

  }

  it must "read an asset address from a json string" in {
    logger.debug("asset address as jsstring: akJsoCcyh34FGPotxfEoSXGwFPCNAkyCgTA")
    logger.debug("testutil address: " + TestUtil.assetAddress)
    val address = JsString(TestUtil.assetAddress.value)
    AddressProtocol.AddressFormat.read(address) must be (TestUtil.assetAddress)
  }

  it must "throw an exception if it receives an invalid address" in {
    val address = JsString("c123")

    intercept[RuntimeException] {
      AddressProtocol.AddressFormat.read(address) must be (TestUtil.assetAddress)
    }
  }

  it must "write a bitcoin address" in {
    AddressProtocol.AddressFormat.write(TestUtil.bitcoinAddress).fields("address") must be (JsString(TestUtil.bitcoinAddress.value))
  }


}
