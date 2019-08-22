package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class TransactionMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.transactionMessage) { txMsg =>
      assert(TransactionMessage.fromHex(txMsg.hex) == txMsg)
    }
  }

  it must "have a meaningful toString" in {
    forAll(DataMessageGenerator.transactionMessage) { txMsg =>
      assert(txMsg.toString.length < 120)
    }
  }

}
