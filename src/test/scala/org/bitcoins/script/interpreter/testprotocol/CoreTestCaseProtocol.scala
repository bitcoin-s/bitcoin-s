package org.bitcoins.script.interpreter.testprotocol

import org.bitcoins.marshallers.script.ScriptParser
import org.bitcoins.marshallers.script.ScriptPubKeyMarshaller.ScriptPubKeyFormatter
import org.bitcoins.marshallers.script.ScriptSignatureMarshaller.ScriptSignatureFormatter
import org.bitcoins.protocol.script._
import org.bitcoins.script.constant.{ScriptOperation, ScriptToken}
import org.bitcoins.util.{BitcoinSLogger, BitcoinSUtil}
import org.slf4j.LoggerFactory
import spray.json._

/**
 * Created by chris on 1/18/16.
 */
object CoreTestCaseProtocol extends DefaultJsonProtocol with BitcoinSLogger {

  implicit object CoreTestCaseFormatter extends RootJsonFormat[Option[CoreTestCase]] {

    override def read(value : JsValue) : Option[CoreTestCase] = {
      logger.debug("Test case: " + value)
      val jsArray : JsArray = value match {
        case array : JsArray => array
        case _ => throw new RuntimeException("Core test case must be in the format of js array")
      }
      val elements : Vector[JsValue]  = jsArray.elements
      if (elements.size < 3) {
        //means that the line is probably a separator between different types of test cases i.e.
        //["Equivalency of different numeric encodings"]
        None
      } else if (elements.size == 3) {

        val scriptPubKeyAsm = parseScriptPubKeyAsm(elements(1))
        val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyAsm)
        val scriptPubKeyCoreTestCase = ScriptPubKeyCoreTestCaseImpl(scriptPubKeyAsm, scriptPubKey)
        val scriptSignatureAsm : Seq[ScriptToken] = parseScriptSignatureAsm(elements.head)
        val scriptSignature : ScriptSignature = ScriptSignature(scriptSignatureAsm,scriptPubKey)
        val scriptSignatureCoreTestCase = ScriptSignatureCoreTestCaseImpl(scriptSignatureAsm,scriptSignature)

        val flags = elements(2).convertTo[String]
        Some(CoreTestCaseImpl(scriptSignatureCoreTestCase,scriptPubKeyCoreTestCase,flags,"No comments from bitcoin core ",elements.toString))
      } else if (elements.size == 4) {
        val scriptPubKeyAsm : Seq[ScriptToken] = parseScriptPubKeyAsm(elements(1))
        val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyAsm)
        val scriptPubKeyCoreTestCase = ScriptPubKeyCoreTestCaseImpl(scriptPubKeyAsm, scriptPubKey)
        val scriptSignatureAsm : Seq[ScriptToken] = parseScriptSignatureAsm(elements.head)
        val scriptSignature : ScriptSignature = ScriptSignature(scriptSignatureAsm,scriptPubKey)
        val scriptSignatureCoreTestCase = ScriptSignatureCoreTestCaseImpl(scriptSignatureAsm,scriptSignature)
        val flags = elements(2).convertTo[String]
        val comments = elements(3).convertTo[String]
        Some(CoreTestCaseImpl(scriptSignatureCoreTestCase,scriptPubKeyCoreTestCase,flags,comments, elements.toString))
      } else None

    }


    /**
     * Parses the script signature asm, it can come in multiple formats
     * such as byte strings i.e. 0x02 0x01 0x00
     * and numbers   1  2
     * look at scirpt_valid.json file for example formats
 *
     * @param element
     * @return
     */
    private def parseScriptSignatureAsm(element : JsValue) : Seq[ScriptToken] = {
      ScriptParser.fromString(element.convertTo[String])
    }


    /**
     * Parses a script pubkey asm from the bitcoin core test cases,
     * example formats:
     * "2 EQUALVERIFY 1 EQUAL"
     * "'Az' EQUAL"
     * look at scirpt_valid.json file for more example formats
 *
     * @param element
     * @return
     */
    private def parseScriptPubKeyAsm(element : JsValue) : Seq[ScriptToken] = {
      ScriptParser.fromString(element.convertTo[String])
    }

    override def write(coreTestCase : Option[CoreTestCase]) : JsValue = ???
  }

}
