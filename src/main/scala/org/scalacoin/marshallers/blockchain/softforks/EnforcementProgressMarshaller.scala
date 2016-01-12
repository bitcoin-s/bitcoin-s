package org.scalacoin.marshallers.blockchain.softforks

import org.scalacoin.protocol.blockchain.softforks.{EnforcementProgressImpl, EnforcementProgress}
import spray.json._

/**
 * Created by Tom on 1/11/2016.
 */
object EnforcementProgressMarshaller extends DefaultJsonProtocol {
  val statusKey = "status"
  val newVersionBlocksFoundKey = "found"
  val requiredBlocksKey = "required"
  val recentBlocksWindowKey = "window"

  implicit object EnforcementProgressFormatter extends RootJsonFormat[EnforcementProgress] {
    override def read (value : JsValue) : EnforcementProgress = {
      val obj = value.asJsObject
      val status = obj.fields(statusKey).convertTo[Boolean]
      val newVersionBlocksFound = obj.fields(newVersionBlocksFoundKey).convertTo[Int]
      val requiredBlocks = obj.fields(requiredBlocksKey).convertTo[Int]
      val recentBlocksWindow = obj.fields(recentBlocksWindowKey).convertTo[Int]
      EnforcementProgressImpl(status, newVersionBlocksFound, requiredBlocks, recentBlocksWindow)
    }

    override def write (enforce : EnforcementProgress) : JsValue = {

      val m : Map[String, JsValue] = Map (
        statusKey -> JsBoolean(enforce.status),
        newVersionBlocksFoundKey -> JsNumber(enforce.newVersionBlocksFound),
        requiredBlocksKey -> JsNumber(enforce.requiredBlocks),
        recentBlocksWindowKey -> JsNumber(enforce.recentBlocksWindow)
      )
      JsObject(m)
    }
  }
}
