package org.scalacoin.script.interpreter.testprotocol

import org.scalacoin.marshallers.script.ScriptParser
import org.scalacoin.marshallers.script.ScriptPubKeyMarshaller.ScriptPubKeyFormatter
import org.scalacoin.marshallers.script.ScriptSignatureMarshaller.ScriptSignatureFormatter
import org.scalacoin.protocol.script.{ScriptSignatureImpl, ScriptPubKeyFactory, ScriptSignatureFactory, ScriptSignature}
import org.scalacoin.script.constant.{ScriptOperation, ScriptToken}
import org.scalacoin.util.ScalacoinUtil
import org.slf4j.LoggerFactory
import spray.json._

/**
 * Created by chris on 1/18/16.
 */
object CoreTestCaseProtocol extends DefaultJsonProtocol {
  private lazy val logger = LoggerFactory.getLogger(this.getClass().toString())
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
        val scriptSignatureAsm : Seq[ScriptToken] = parseScriptSignatureAsm(elements.head)
        val scriptSignature : ScriptSignature = ScriptSignatureImpl(scriptSignatureAsm, scriptSignatureAsm.map(_.hex).mkString)
        val scriptPubKeyAsm = parseScriptPubKeyAsm(elements(1))
        val scriptPubKey = ScriptPubKeyFactory.factory(scriptPubKeyAsm)
        val flags = elements(2).convertTo[String]
        Some(CoreTestCaseImpl(scriptSignature,scriptPubKey,flags,"No comments from bitcoin core ",elements.toString))
      } else if (elements.size == 4) {
        val scriptSignatureAsm : Seq[ScriptToken] = parseScriptSignatureAsm(elements.head)
        val scriptSignature : ScriptSignature = ScriptSignatureImpl(scriptSignatureAsm, scriptSignatureAsm.map(_.hex).mkString)
        val scriptPubKeyAsm : Seq[ScriptToken] = parseScriptPubKeyAsm(elements(1))
        val scriptPubKey = ScriptPubKeyFactory.factory(scriptPubKeyAsm)
        val flags = elements(2).convertTo[String]
        val comments = elements(3).convertTo[String]
        Some(CoreTestCaseImpl(scriptSignature,scriptPubKey,flags,comments, elements.toString))
      } else None

    }


    /**
     * Parses the script signature asm, it can come in multiple formats
     * such as byte strings i.e. 0x02 0x01 0x00
     * and numbers   1  2
     * look at scirpt_valid.json file for example formats
     * @param elements
     * @return
     */
    private def parseScriptSignatureAsm(element : JsValue) : Seq[ScriptToken] = {
      ScriptParser.parse(element.convertTo[String])
    }


    /**
     * Parses a script pubkey asm from the bitcoin core test cases,
     * example formats:
     * "2 EQUALVERIFY 1 EQUAL"
     * "'Az' EQUAL"
     * look at scirpt_valid.json file for more example formats
     * @param element
     * @return
     */
    private def parseScriptPubKeyAsm(element : JsValue) : Seq[ScriptToken] = {
      try {
        ScriptParser.parse(ScalacoinUtil.decodeHex(element.convertTo[String].toLowerCase))
      } catch {
        case _ : Throwable => ScriptParser.parse(element.convertTo[String])
      }

    }





    override def write(coreTestCase : Option[CoreTestCase]) : JsValue = ???
  }

}
