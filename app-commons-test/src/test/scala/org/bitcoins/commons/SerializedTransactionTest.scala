package org.bitcoins.commons

import org.bitcoins.commons.jsonmodels.SerializedTransaction
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  FieldElement,
  SchnorrDigitalSignature,
  SchnorrNonce
}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class SerializedTransactionTest extends BitcoinSUnitTest {

  behavior of "SerializedTransaction.decodeRawTransaction"

  it must "correctly decode a transaction with TaprootKeyPath witness" in {
    // Create a minimal witness transaction with a Taproot key path spend
    // This simulates a real taproot transaction with witness data

    // Create a Schnorr signature for Taproot key path spend
    val schnorrSig = SchnorrDigitalSignature(
      SchnorrNonce.fromHex(
        "25e45bd4d4b8fcd5933861355a2d376aad8daf1af1588e5fb6dfcea22d0d809a"
      ),
      FieldElement.fromHex(
        "cda6fadca11e97f5b5c85af99df27cb24fa69b08fa6c790234cdc671d3af5a73"
      ),
      None
    )
    val taprootKeyPathWitness = TaprootKeyPath(schnorrSig)

    // Create a dummy transaction input
    val prevTxId = DoubleSha256DigestBE.fromHex(
      "eb5ecb00000000925977cc01f9875c000000000016001431d2b00cd4687ceb34"
    )
    val input = TransactionInput(
      outPoint = TransactionOutPoint(prevTxId.flip, UInt32.zero),
      scriptSignature = EmptyScriptSignature,
      sequenceNumber = UInt32.max
    )

    // Create a dummy output
    val output = TransactionOutput(
      value = Satoshis(10000),
      scriptPubKey = EmptyScriptPubKey
    )

    // Create witness transaction
    val wtx = WitnessTransaction(
      version = Int32.two,
      inputs = Vector(input),
      outputs = Vector(output),
      lockTime = UInt32.zero,
      witness = TransactionWitness(Vector(taprootKeyPathWitness))
    )

    val decoded = SerializedTransaction.decodeRawTransaction(wtx)

    // Verify basic transaction properties
    assert(decoded.vin.size == 1)
    assert(decoded.vout.size == 1)

    // Verify the witness is present and correctly decoded
    val decodedInput = decoded.vin.head
    assert(decodedInput.txinwitness.isDefined, "Witness should be present")

    val witness = decodedInput.txinwitness.get
    // Verify scriptType is TaprootKeyPath
    assert(
      witness.scriptType.contains("TaprootKeyPath"),
      s"Expected TaprootKeyPath but got ${witness.scriptType}"
    )

    // Verify the witness hex is populated
    assert(witness.hex == taprootKeyPathWitness.hex)

    // Verify the stack is present and has one element (signature for key path spend)
    assert(witness.stack.isDefined, "Stack should be present")
    assert(
      witness.stack.get.size == 1,
      s"Stack should have 1 element but has ${witness.stack.get.size}"
    )

    // The stack should contain the schnorr signature bytes
    assert(witness.stack.get.head == schnorrSig.bytes)

    // For TaprootKeyPath, script, pubKey, and signature should be None
    assert(witness.script.isEmpty)
    assert(witness.pubKey.isEmpty)
    assert(witness.signature.isEmpty)
  }
}

