package org.bitcoins.marshallers.blockchain.softforks

import org.bitcoins.marshallers.MarshallerUtil
import org.bitcoins.protocol.blockchain.softforks.{SoftForksImpl, RejectionProgress, EnforcementProgress, SoftForks}
import spray.json._
import RejectionProgressMarshaller._
import EnforcementProgressMarshaller._

/**
 * Created by Tom on 1/11/2016.
 */
object SoftForkMarshaller extends DefaultJsonProtocol with MarshallerUtil {
  val idKey = "id"
  val versionKey = "version"
  val enforceKey = "enforce"
  val rejectKey = "reject"

  implicit object SoftForkFormatter extends RootJsonFormat[SoftForks] {
    override def read (value : JsValue) : SoftForks = {
      val obj = value.asJsObject
      val id = obj.fields(idKey).convertTo[String]
      val version = obj.fields(versionKey).convertTo[Int]
      val enforce : EnforcementProgress = EnforcementProgressFormatter.read(obj.fields(enforceKey))
      val reject : RejectionProgress = RejectionProgressFormatter.read(obj.fields(rejectKey))
      SoftForksImpl(id, version, enforce, reject)
    }

    override def write (detail: SoftForks) : JsValue = {

      val enforce : JsValue = EnforcementProgressFormatter.write(detail.enforce)
      val reject : JsValue = RejectionProgressFormatter.write(detail.reject)

      val m : Map[String, JsValue] = Map (
        idKey -> JsString(detail.id),
        versionKey -> JsNumber(detail.version),
        enforceKey -> enforce,
        rejectKey -> reject
      )
      JsObject(m)
    }
  }
}
