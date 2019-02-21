package org.bitcoins.node.messages.data

import org.bitcoins.node.gen.DataMessageGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 9/1/16.
  */
class TransactionMessageSpec extends Properties("TransactionMessageSpec") {

  property("serialization symmetry") = {
    Prop.forAll(DataMessageGenerator.transactionMessage) {
      case txMsg =>
        TransactionMessage.fromHex(txMsg.hex) == txMsg
    }
  }

}
