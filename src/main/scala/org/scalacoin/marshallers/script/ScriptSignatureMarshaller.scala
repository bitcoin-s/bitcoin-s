package org.scalacoin.marshallers.script

import org.scalacoin.protocol.script.ScriptSignatureImpl
import spray.json.DefaultJsonProtocol

/**
 * Created by chris on 12/27/15.
 */
object ScriptSignatureMarshaller extends DefaultJsonProtocol {

  implicit val scriptSignatureMarshaller = jsonFormat2(ScriptSignatureImpl.apply _)
}
