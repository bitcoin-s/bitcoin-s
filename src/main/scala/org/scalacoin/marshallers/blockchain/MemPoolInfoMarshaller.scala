package org.scalacoin.marshallers.blockchain

import org.scalacoin.protocol.blockchain.{MemPoolInfo, MemPoolInfoImpl}
import spray.json._


/**
 * Created by Tom on 1/11/2016.
 */
object MemPoolInfoMarshaller extends DefaultJsonProtocol {
  val sizeKey = "size"
  val bytesKey = "bytes"

  implicit object MemPoolInfoFormatter extends RootJsonFormat[MemPoolInfo] {
    override def read (value : JsValue) : MemPoolInfo = {
      val obj = value.asJsObject
      val size = obj.fields(sizeKey).convertTo[Int]
      val bytes = obj.fields(bytesKey).convertTo[Int]
      MemPoolInfoImpl(size, bytes)
    }
    override def write (mempool : MemPoolInfo) : JsValue = {
      val m : Map[String, JsValue] = Map (
        sizeKey -> JsNumber(mempool.size),
        bytesKey -> JsNumber(mempool.bytes)
      )
      JsObject(m)
    }
  }
}
