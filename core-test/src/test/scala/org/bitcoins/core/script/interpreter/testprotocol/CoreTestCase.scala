package org.bitcoins.core.script.interpreter.testprotocol

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.script.result.ScriptResult
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util.{BitcoinScriptUtil, BytesUtil}
import scodec.bits._
import ujson._
import upickle.default._

/** Created by chris on 1/18/16.
  * This represents a core test case for valid and invalid scripts
  * the scripts can be seen in the script_tests.json file
  * files.
  */
case class CoreTestCase(
    scriptSig: ScriptSignature,
    scriptPubKey: ScriptPubKey,
    flags: String,
    expectedResult: ScriptResult,
    comments: String,
    raw: String,
    witness: Option[(ScriptWitness, CurrencyUnit)])

object CoreTestCase {

  implicit val coreTestCaseR: Reader[Option[CoreTestCase]] = reader[Value].map {
    value =>
      val arr: Arr = value match {
        case array: Arr => array
        case _ =>
          throw new RuntimeException(
            "Core test case must be in the format of js array")
      }
      val elements: Vector[Value] = arr.value.toVector
      if (elements.size < 3) {
        //means that the line is probably a separator between different types of test cases i.e.
        //["Equivalency of different numeric encodings"]
        None
      } else if (elements.size == 4) {
        //means we are missing a comment
        val scriptPubKeyBytes: ByteVector = parseScriptPubKey(elements(1))
        val scriptPubKey = ScriptPubKey(scriptPubKeyBytes)
        val scriptSignatureBytes: ByteVector =
          parseScriptSignature(elements.head)
        val scriptSignature: ScriptSignature =
          ScriptSignature(scriptSignatureBytes)
        val flags = elements(2).str
        val expectedResult = ScriptResult(elements(3).str)
        Some(
          CoreTestCase(scriptSignature,
                       scriptPubKey,
                       flags,
                       expectedResult,
                       "",
                       elements.toString,
                       None))
      } else if (elements.size == 5 && elements.head.isInstanceOf[Arr]) {
        //means we have a witness as the first item in our array
        val witnessArray = elements.head.asInstanceOf[Arr]
        val amount = Satoshis((witnessArray.value.last.num * 100000000L).toLong)
        val stack = witnessArray.value.toVector
          .slice(0, witnessArray.value.size - 1)
          .map(c => BytesUtil.decodeHex(c.str))
        val witness = ScriptWitness(stack.reverse)
        val scriptPubKeyBytes: ByteVector = parseScriptPubKey(elements(2))
        val scriptPubKey = ScriptPubKey(scriptPubKeyBytes)
        val scriptSignatureBytes: ByteVector = parseScriptSignature(elements(1))
        val scriptSignature: ScriptSignature =
          ScriptSignature(scriptSignatureBytes)
        val flags = elements(3).str
        val expectedResult = ScriptResult(elements(4).str)
        Some(
          CoreTestCase(scriptSignature,
                       scriptPubKey,
                       flags,
                       expectedResult,
                       "",
                       elements.toString,
                       Some((witness, amount))))
      } else if (elements.size == 5) {
        val scriptPubKeyBytes: ByteVector = parseScriptPubKey(elements(1))
        val scriptPubKey = ScriptPubKey(scriptPubKeyBytes)
        val scriptSignatureBytes: ByteVector =
          parseScriptSignature(elements.head)
        val scriptSignature: ScriptSignature =
          ScriptSignature(scriptSignatureBytes)
        val flags = elements(2).str
        val expectedResult = ScriptResult(elements(3).str)
        val comments = elements(4).str
        Some(
          CoreTestCase(scriptSignature,
                       scriptPubKey,
                       flags,
                       expectedResult,
                       comments,
                       elements.toString,
                       None))
      } else if (elements.size == 6 && elements.head.arrOpt.isDefined) {
        val witnessArray = elements.head.arr
        val amount = Satoshis((witnessArray.value.last.num * 100000000L).toLong)
        val stack = witnessArray.value.toVector
          .slice(0, witnessArray.value.size - 1)
          .map(c => BytesUtil.decodeHex(c.str))
        val witness = ScriptWitness(stack.reverse)
        val scriptPubKeyBytes: ByteVector = parseScriptPubKey(elements(2))
        val scriptPubKey = ScriptPubKey(scriptPubKeyBytes)
        val scriptSignatureBytes: ByteVector = parseScriptSignature(elements(1))
        val scriptSignature: ScriptSignature =
          ScriptSignature(scriptSignatureBytes)
        val flags = elements(3).str
        val expectedResult = ScriptResult(elements(4).str)
        val comments = elements(5).str
        Some(
          CoreTestCase(scriptSignature,
                       scriptPubKey,
                       flags,
                       expectedResult,
                       comments,
                       elements.toString,
                       Some((witness, amount))))
      } else None
  }

  /** Parses the script signature asm, it can come in multiple formats
    * such as byte strings i.e. 0x02 0x01 0x00
    * and numbers   1  2
    * look at scirpt_valid.json file for example formats
    */
  private def parseScriptSignature(element: Value): ByteVector = {
    val asm = ScriptParser.fromString(element.str)
    val bytes = BitcoinScriptUtil.asmToBytes(asm)
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(bytes)
    compactSizeUInt.bytes ++ bytes
  }

  /** Parses a script pubkey asm from the bitcoin core test cases,
    * example formats:
    * "2 EQUALVERIFY 1 EQUAL"
    * "'Az' EQUAL"
    * look at scirpt_valid.json file for more example formats
    */
  private def parseScriptPubKey(element: Value): ByteVector = {
    val asm = ScriptParser.fromString(element.str)
    val bytes = BitcoinScriptUtil.asmToBytes(asm)
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(bytes)
    compactSizeUInt.bytes ++ bytes
  }
}
