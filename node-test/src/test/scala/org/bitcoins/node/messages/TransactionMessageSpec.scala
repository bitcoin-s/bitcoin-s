package org.bitcoins.node.messages

import org.bitcoins.testkit.gen.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class TransactionMessageSpec extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.transactionMessage) { txMsg =>
      assert(TransactionMessage.fromHex(txMsg.hex) == txMsg)
    }
  }

}
