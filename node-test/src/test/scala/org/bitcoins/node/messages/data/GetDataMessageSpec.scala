package org.bitcoins.node.messages.data

import org.bitcoins.testkit.gen.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 7/8/16.
  */
class GetDataMessageSpec extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.getDataMessages) { dataMsg =>
      assert(GetDataMessage(dataMsg.hex) == dataMsg)
    }
  }
}
