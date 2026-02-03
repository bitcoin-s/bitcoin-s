package org.bitcoins.commons

import org.bitcoins.commons.jsonmodels.SerializedTransaction
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits._

class SerializedTransactionTest extends BitcoinSUnitTest {

  behavior of "SerializedTransaction.decodeRawTransaction"

  it must "correctly decode a transaction with TaprootKeyPath witness" in {
    // Transaction hex from TaprootTxTests with a TaprootKeyPath witness
    // This is a real taproot key path spend transaction
    val txHex =
      "f705d6e8019870958e85d1d8f94aa6d74746ba974db0f5ccae49a49b32dcada4e19de4eb5ecb00000000925977cc01f9875c000000000016001431d2b00cd4687ceb34008d9894de84062def14aa05406346"
    
    val tx = Transaction.fromHex(txHex)
    val decoded = SerializedTransaction.decodeRawTransaction(tx)

    // Verify basic transaction properties
    assert(decoded.vin.size == 1)
    assert(decoded.vout.size == 1)

    // Verify the witness is present and correctly decoded
    val input = decoded.vin.head
    assert(input.txinwitness.isDefined)

    val witness = input.txinwitness.get
    // Verify scriptType is TaprootKeyPath
    assert(witness.scriptType.contains("TaprootKeyPath"))

    // Verify the witness hex is populated
    val expectedWitnessHex =
      "0125e45bd4d4b8fcd5933861355a2d376aad8daf1af1588e5fb6dfcea22d0d809acda6fadca11e97f5b5c85af99df27cb24fa69b08fa6c790234cdc671d3af5a7302"
    assert(witness.hex == expectedWitnessHex)

    // Verify the stack is present and has one element (signature for key path spend)
    assert(witness.stack.isDefined)
    assert(witness.stack.get.size == 1)
    
    val expectedStackElement =
      hex"25e45bd4d4b8fcd5933861355a2d376aad8daf1af1588e5fb6dfcea22d0d809acda6fadca11e97f5b5c85af99df27cb24fa69b08fa6c790234cdc671d3af5a7302"
    assert(witness.stack.get.head == expectedStackElement)

    // For TaprootKeyPath, script, pubKey, and signature should be None
    assert(witness.script.isEmpty)
    assert(witness.pubKey.isEmpty)
    assert(witness.signature.isEmpty)
  }
}
