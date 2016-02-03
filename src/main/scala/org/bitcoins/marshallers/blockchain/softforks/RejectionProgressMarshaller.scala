package org.bitcoins.marshallers.blockchain.softforks

import org.bitcoins.protocol.blockchain.softforks.{RejectionProgressImpl, RejectionProgress}
import spray.json._

/**
 * Created by Tom on 1/11/2016.
 */
object RejectionProgressMarshaller extends DefaultJsonProtocol {
  val statusKey = "status"
  val newVersionBlocksFoundKey = "found"
  val requiredBlocksKey = "required"
  val recentBlocksWindowKey = "window"

  implicit object RejectionProgressFormatter extends RootJsonFormat[RejectionProgress] {
    override def read (value : JsValue) : RejectionProgress = {
      val obj = value.asJsObject
      val status = obj.fields(statusKey).convertTo[Boolean]
      val newVersionBlocksFound = obj.fields(newVersionBlocksFoundKey).convertTo[Int]
      val requiredBlocks = obj.fields(requiredBlocksKey).convertTo[Int]
      val recentBlocksWindow = obj.fields(recentBlocksWindowKey).convertTo[Int]
      RejectionProgressImpl(status, newVersionBlocksFound, requiredBlocks, recentBlocksWindow)
    }

    override def write (reject : RejectionProgress) : JsValue = {

      val m : Map[String, JsValue] = Map (
        statusKey -> JsBoolean(reject.status),
        newVersionBlocksFoundKey -> JsNumber(reject.newVersionBlocksFound),
        requiredBlocksKey -> JsNumber(reject.requiredBlocks),
        recentBlocksWindowKey -> JsNumber(reject.recentBlocksWindow)
      )
      JsObject(m)
    }
  }
}
