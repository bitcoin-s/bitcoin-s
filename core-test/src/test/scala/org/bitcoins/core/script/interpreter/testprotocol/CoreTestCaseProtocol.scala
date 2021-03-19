package org.bitcoins.core.script.interpreter.testprotocol

import grizzled.slf4j.Logging
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.script.result.ScriptResult
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util.{BitcoinScriptUtil, BytesUtil}
import scodec.bits.ByteVector
import spray.json._

/** Created by chris on 1/18/16.
  */
object CoreTestCaseProtocol extends DefaultJsonProtocol with Logging {

  implicit object CoreTestCaseFormatter
      extends RootJsonFormat[Option[CoreTestCase]] {

    override def read(value: JsValue): Option[CoreTestCase] = {
      val jsArray: JsArray = value match {
        case array: JsArray => array
        case _ =>
          throw new RuntimeException(
            "Core test case must be in the format of js array")
      }
      val elements: Vector[JsValue] = jsArray.elements
      if (elements.size < 3) {
        //means that the line is probably a separator between different types of test cases i.e.
        //["Equivalency of different numeric encodings"]
        None
      } else if (elements.size == 4) {
        //means we are missing a comment
        val scriptPubKeyBytes: ByteVector = parseScriptPubKey(elements(1))
        val scriptPubKey = ScriptPubKey(scriptPubKeyBytes)
        val scriptSignatureBytes: ByteVector = parseScriptSignature(
          elements.head)
        val scriptSignature: ScriptSignature = ScriptSignature(
          scriptSignatureBytes)
        val flags = elements(2).convertTo[String]
        val expectedResult = ScriptResult(elements(3).convertTo[String])
        Some(
          CoreTestCaseImpl(scriptSignature,
                           scriptPubKey,
                           flags,
                           expectedResult,
                           "",
                           elements.toString,
                           None))
      } else if (elements.size == 5 && elements.head.isInstanceOf[JsArray]) {
        //means we have a witness as the first item in our array
        val witnessArray = elements.head.asInstanceOf[JsArray]
        val amount = Satoshis(
          (witnessArray.elements.last.convertTo[Double] * 100000000L).toLong)
        val stack = witnessArray.elements
          .slice(0, witnessArray.elements.size - 1)
          .map(c => BytesUtil.decodeHex(c.convertTo[String]))
        val witness = ScriptWitness(stack.reverse)
        val scriptPubKeyBytes: ByteVector = parseScriptPubKey(elements(2))
        val scriptPubKey = ScriptPubKey(scriptPubKeyBytes)
        val scriptSignatureBytes: ByteVector = parseScriptSignature(elements(1))
        val scriptSignature: ScriptSignature = ScriptSignature(
          scriptSignatureBytes)
        val flags = elements(3).convertTo[String]
        val expectedResult = ScriptResult(elements(4).convertTo[String])
        Some(
          CoreTestCaseImpl(scriptSignature,
                           scriptPubKey,
                           flags,
                           expectedResult,
                           "",
                           elements.toString,
                           Some((witness, amount))))
      } else if (elements.size == 5) {
        val scriptPubKeyBytes: ByteVector = parseScriptPubKey(elements(1))
        val scriptPubKey = ScriptPubKey(scriptPubKeyBytes)
        val scriptSignatureBytes: ByteVector = parseScriptSignature(
          elements.head)
        val scriptSignature: ScriptSignature = ScriptSignature(
          scriptSignatureBytes)
        val flags = elements(2).convertTo[String]
        val expectedResult = ScriptResult(elements(3).convertTo[String])
        val comments = elements(4).convertTo[String]
        Some(
          CoreTestCaseImpl(scriptSignature,
                           scriptPubKey,
                           flags,
                           expectedResult,
                           comments,
                           elements.toString,
                           None))
      } else if (elements.size == 6 && elements.head.isInstanceOf[JsArray]) {
        val witnessArray = elements.head.asInstanceOf[JsArray]
        val amount = Satoshis(
          (witnessArray.elements.last.convertTo[Double] * 100000000L).toLong)
        val stack = witnessArray.elements
          .slice(0, witnessArray.elements.size - 1)
          .map(c => BytesUtil.decodeHex(c.convertTo[String]))
        val witness = ScriptWitness(stack.reverse)
        val scriptPubKeyBytes: ByteVector = parseScriptPubKey(elements(2))
        val scriptPubKey = ScriptPubKey(scriptPubKeyBytes)
        val scriptSignatureBytes: ByteVector = parseScriptSignature(elements(1))
        val scriptSignature: ScriptSignature = ScriptSignature(
          scriptSignatureBytes)
        val flags = elements(3).convertTo[String]
        val expectedResult = ScriptResult(elements(4).convertTo[String])
        val comments = elements(5).convertTo[String]
        Some(
          CoreTestCaseImpl(scriptSignature,
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
      *
      * @param element
      * @return
      */
    private def parseScriptSignature(element: JsValue): ByteVector = {
      val asm = ScriptParser.fromString(element.convertTo[String])
      val bytes = BitcoinScriptUtil.asmToBytes(asm)
      val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(bytes)
      compactSizeUInt.bytes ++ bytes
    }

    /** Parses a script pubkey asm from the bitcoin core test cases,
      * example formats:
      * "2 EQUALVERIFY 1 EQUAL"
      * "'Az' EQUAL"
      * look at scirpt_valid.json file for more example formats
      * @param element
      * @return
      */
    private def parseScriptPubKey(element: JsValue): ByteVector = {
      val asm = ScriptParser.fromString(element.convertTo[String])
      val bytes = BitcoinScriptUtil.asmToBytes(asm)
      val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(bytes)
      compactSizeUInt.bytes ++ bytes
    }

    override def write(coreTestCase: Option[CoreTestCase]): JsValue = ???
  }

}
