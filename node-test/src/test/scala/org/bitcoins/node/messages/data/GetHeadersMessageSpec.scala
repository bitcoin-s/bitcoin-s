package org.bitcoins.node.messages.data

import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.testkit.gen.DataMessageGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/29/16.
  */
class GetHeadersMessageSpec
    extends Properties("GetHeadersMessageSpec")
    with BitcoinSLogger {

  property("Serialization symmetry") =
    Prop.forAll(DataMessageGenerator.getHeaderMessages) { headerMsg =>
      GetHeadersMessage(headerMsg.hex) == headerMsg
    }
}
