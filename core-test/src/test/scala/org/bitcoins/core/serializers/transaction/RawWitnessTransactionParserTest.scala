package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

class RawWitnessTransactionParserTest extends BitcoinSUnitTest {

  "RawWitnessTransactionParser" must "serialize and deserialize a wtx" in {
    val hex =
      "0100000000010115e180dc28a2327e687facc33f10f2a20da717e5548406f7ae8b4c811072f85603000000171600141d7cd6c75c2e86f4cbf98eaed221b30bd9a0b928ffffffff019caef505000000001976a9141d7cd6c75c2e86f4cbf98eaed221b30bd9a0b92888ac02483045022100f764287d3e99b1474da9bec7f7ed236d6c81e793b20c4b5aa1f3051b9a7daa63022016a198031d5554dbb855bdbe8534776a4be6958bd8d530dc001c32b828f6f0ab0121038262a6c6cec93c2d3ecd6c6072efea86d02ff8e3328bbd0242b20af3425990ac00000000"
    val wtx = WitnessTransaction(hex)
    val bytes = wtx.bytes
    BytesUtil.encodeHex(bytes) must be(hex)
  }
}
