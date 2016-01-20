package org.scalacoin.script.interpreter.testprotocol

import org.scalacoin.marshallers.script.ScriptParser
import org.scalacoin.marshallers.script.ScriptPubKeyMarshaller.ScriptPubKeyFormatter
import org.scalacoin.marshallers.script.ScriptSignatureMarshaller.ScriptSignatureFormatter
import org.scalacoin.protocol.script.{ScriptPubKeyFactory, ScriptSignatureFactory, ScriptSignature}
import org.scalacoin.script.constant.ScriptToken
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
      val elements = jsArray.elements
      if (elements.size < 3) {
        //means that the line is probably a separator between different types of test cases i.e.
        //["Equivalency of different numeric encodings"]
        None
      } else if (elements.size == 3) {
        val scriptSignatureAsm : Seq[ScriptToken] = ScriptParser.parse(elements.head.convertTo[String])
        val scriptSignature : ScriptSignature = ScriptSignatureFactory.factory(scriptSignatureAsm)
        val scriptPubKeyAsm : Seq[ScriptToken] = ScriptParser.parse(elements(1).convertTo[String])
        val scriptPubKey = ScriptPubKeyFactory.factory(scriptPubKeyAsm)
        val flags = elements(2).convertTo[String]
        Some(CoreTestCaseImpl(scriptSignature,scriptPubKey,flags,"No comments from bitcoin core ",elements.toString))
      } else if (elements.size == 4) {
        val scriptSignatureAsm : Seq[ScriptToken] = ScriptParser.parse(elements.head.convertTo[String])
        val scriptSignature : ScriptSignature = ScriptSignatureFactory.factory(scriptSignatureAsm)
        val scriptPubKeyAsm : Seq[ScriptToken] = ScriptParser.parse(elements(1).convertTo[String])
        val scriptPubKey = ScriptPubKeyFactory.factory(scriptPubKeyAsm)
        val flags = elements(2).convertTo[String]
        val comments = elements(3).convertTo[String]
        Some(CoreTestCaseImpl(scriptSignature,scriptPubKey,flags,comments, elements.toString))
      } else None

    }

    override def write(coreTestCase : Option[CoreTestCase]) : JsValue = ???
  }

}
