package org.bitcoins.marshallers

import org.bitcoins.marshallers.transaction.TransactionInputMarshaller.TransactionInputFormatter
import org.bitcoins.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json.JsArray

/**
 * Created by chris on 3/31/16.
 */
class MarshallerUtilTest extends FlatSpec with MustMatchers with MarshallerUtil {

  "MarshallerUtil" must "convert a sequence of something to a json array" in {
    val inputs = TestUtil.simpleTransaction.inputs
    val jsonInputs = convertToJsArray(inputs)(TransactionInputFormatter)
    val isJsArray = jsonInputs match {
      case j : JsArray => true
      case _ => false
    }

    isJsArray must be (true)
  }

}
