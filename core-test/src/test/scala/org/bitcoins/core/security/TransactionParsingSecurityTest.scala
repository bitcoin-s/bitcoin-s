package org.bitcoins.core.security

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.core.serializers.blockchain.RawBlockSerializer
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.*

import scala.util.Try

/** Security reproduction tests for transaction, witness and CompactSizeUInt
  * parsing. Each test asserts the CORRECT/SAFE behavior, so every test here
  * FAILS until the corresponding bug is fixed. The failing tests are the fix
  * checklist.
  */
class TransactionParsingSecurityTest extends BitcoinSUnitTest {

  // genesis block coinbase tx, same fixture as RawBlockSerializerTest
  // https://en.bitcoin.it/wiki/Genesis_block
  private val genesisCoinbaseTx =
    "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4" +
      "d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e2062726" +
      "96e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678" +
      "afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c3" +
      "84df7ba0b8d578a4c702b6bf11d5fac00000000"

  private val genesisBlockHeader =
    "01000000" + // version
      "0000000000000000000000000000000000000000000000000000000000000000" + // prev block hash
      "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a" + // merkle root
      "29ab5f49" + // timestamp
      "ffff001d" + // nbits
      "1dac2b7c" // nonce

  private val genesisBlock = genesisBlockHeader + "01" + genesisCoinbaseTx

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

  it must "reject trailing garbage and truncated fields in transactions, blocks, and scripts" in {
    // Finding 2 (Medium): parsers accept trailing garbage and silently
    // zero-pad/clamp truncated fields:
    // - BaseTransaction.fromBytes takes lockTime as lockTimeBytes.take(4) and
    //   ignores bytes after it
    //   (core/src/main/scala/org/bitcoins/core/protocol/transaction/Transaction.scala:201-212);
    //   WitnessTransaction.fromBytes has the same pattern at lines 330-334.
    // - RawBlockSerializer.read discards the leftover bytes after the last tx
    //   (core/src/main/scala/org/bitcoins/core/serializers/blockchain/RawBlockSerializer.scala:17).
    // - BitcoinScriptUtil.parseScript clamps a declared script length to the
    //   available bytes instead of failing
    //   (core/src/main/scala/org/bitcoins/core/util/BitcoinScriptUtil.scala:664-672).
    // Correct behavior: all of these inputs are rejected.

    // trailing garbage after a valid base transaction
    Transaction.fromHexT(genesisCoinbaseTx + "deadbeef").isFailure must be(true)

    // truncated locktime (last byte removed) must not be zero-padded
    Transaction.fromHexT(genesisCoinbaseTx.dropRight(2)).isFailure must be(true)

    // trailing garbage after a valid block
    Try(RawBlockSerializer.read(genesisBlock + "deadbeef")).isFailure must be(
      true)

    // scriptPubKey declares 25 bytes but only 24 are present
    val truncatedScript =
      "1976a9143b75df7c44a47fed51374aef67bb7e7ae071b0a788"
    Try(
      org.bitcoins.core.serializers.script.RawScriptPubKeyParser
        .read(truncatedScript)).isFailure must be(true)
  }

  it must "fail with a parse error (not a raw index exception) on very short transaction input" in {
    // Finding 3 (Low): Transaction.fromBytes does unchecked bytes(4)/bytes(5)
    // indexing
    // (core/src/main/scala/org/bitcoins/core/protocol/transaction/Transaction.scala:127-128),
    // so input shorter than 6 bytes throws a raw IndexOutOfBoundsException.
    // Correct behavior: a proper parse failure (e.g. IllegalArgumentException).
    val ex = intercept[Exception] {
      Transaction.fromBytes(hex"0100000000")
    }
    ex.isInstanceOf[IndexOutOfBoundsException] must be(false)
  }

  it must "reject truncated and non-canonical CompactSizeUInt varints" in {
    // Finding 4 (Low): CompactSizeUInt.parseCompactSizeUInt
    // (core/src/main/scala/org/bitcoins/core/protocol/CompactSizeUInt.scala:122-136)
    // silently pads truncated multi-byte varints and accepts non-canonical
    // encodings. Correct behavior: both are rejected.

    // 0xfd prefix requires 2 more bytes, only 1 is present
    CompactSizeUInt.fromBytesT(hex"fd01").isFailure must be(true)

    // non-canonical: value 1 must use the 1-byte form, not the 0xfd form
    CompactSizeUInt.fromBytesT(hex"fd0100").isFailure must be(true)

    // non-canonical: value 253 must use the 0xfd form, not the 0xfe form
    CompactSizeUInt.fromBytesT(hex"fefd000000").isFailure must be(true)
  }

  it must "reject witness stack elements whose declared length exceeds the remaining bytes" in {
    // Finding 5 (Low): RawScriptWitnessParser.read clamps the stack element
    // length with ByteVector.take instead of failing
    // (core/src/main/scala/org/bitcoins/core/serializers/script/RawScriptWitnessParser.scala:30),
    // silently mutating witness data. Correct behavior: a declared element
    // length larger than the remaining bytes is rejected.
    // 1 stack item, declared element length 2, only 1 byte available.
    Try(RawScriptWitnessParser.read(hex"0102ff")).isFailure must be(true)
  }

  it must "reject a witness transaction with a flag byte other than 1" in {
    // Finding 6 (Info): WitnessTransaction.fromBytes only requires flag != 0
    // (core/src/main/scala/org/bitcoins/core/protocol/transaction/Transaction.scala:317-323).
    // BIP144 defines the flag as 0x01 and Bitcoin Core rejects any other
    // value. Correct behavior: reject.
    // This is a valid witness tx (txid c586389e5e4b3acb9d6c8be1c19ae8ab2795397633176f5a6442a261bbdefc3a,
    // same fixture as TransactionTest) with the flag byte changed 01 -> 02.
    val invalidFlagTx =
      "02000000" + // version
        "00" + "02" + // marker 0, INVALID flag 2
        "0140d43a99926d43eb0e619bf0b3d83b4a31f60c176beecfb9d35bf45e54d0f7420100000017160014a4b4ca48de0b3fffc15404a1acdc8dbaae226955ffffffff0100e1f5050000000017a9144a1154d50b03292b3024370901711946cb7cccc387024830450221008604ef8f6d8afa892dee0f31259b6ce02dd70c545cfcfed8148179971876c54a022076d771d6e91bed212783c9b06e0de600fab2d518fad6f15a2b191d7fbd262a3e0121039d25ab79f41f75ceaf882411fd41fa670a4c672c23ffaf0e361a969cde0692e800000000"
    WitnessTransaction.fromHexT(invalidFlagTx).isFailure must be(true)
  }
}
