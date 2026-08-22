package org.bitcoins.core.security

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Security reproduction tests for transaction, witness and CompactSizeUInt
  * parsing. Each test asserts the CORRECT/SAFE behavior, so every test here
  * FAILS until the corresponding bug is fixed. The failing tests are the fix
  * checklist.
  */
class TransactionParsingSecurityTest extends BitcoinSUnitTest {

  behavior of "Transaction parsing security"

  it must "reject a malformed witness transaction instead of silently re-parsing it as a base transaction" in {
    // Finding 1 (Medium): the catch-all fallback in Transaction.fromBytes
    // (core/src/main/scala/org/bitcoins/core/protocol/transaction/Transaction.scala:126-146)
    // catches the witness parse failure and re-parses the SAME bytes as a base
    // transaction, silently dropping the witness intent. Correct behavior:
    // parsing fails.
    // The bytes below have a valid marker/flag, 1 input and 1 output, but the
    // witness section declares 2 stack items whose encoding consumes bytes
    // into the locktime, leaving only 3 locktime bytes. WitnessTransaction
    // parsing fails; the fallback then parses the bytes as a base tx
    // (0 inputs, 1 garbage output).
    val malformedWitnessTx =
      "01000000" + // version
        "0001" + // witness marker + flag
        "01" + // 1 input
        "0000000000000000000000000000000000000000000000000000000000000000" + // prev txid
        "00000000" + // vout
        "00" + // empty scriptSig
        "ffffffff" + // sequence
        "01" + // 1 output
        "0000000000000000" + // 0 satoshis
        "00" + // empty scriptPubKey
        "02" + // witness: 2 stack items
        "01" + "aa" + // item 1: len 1
        // item 2 is parsed out of the locktime bytes below
        "00000000" // locktime
    Transaction.fromHexT(malformedWitnessTx).isFailure must be(true)
  }
}
