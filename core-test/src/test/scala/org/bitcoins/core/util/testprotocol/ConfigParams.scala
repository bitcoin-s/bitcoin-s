package org.bitcoins.core.util.testprotocol

import spray.json._

/** Created by tom on 6/14/16.
  */
trait ConfigParams {
  def addrTypeOrIsCompressed: Either[String, Boolean]
  def isPrivKey: Boolean
  def isTestNet: Boolean
}

case class ConfigParamsImpl(
    addrTypeOrIsCompressed: Either[String, Boolean],
    isPrivKey: Boolean,
    isTestNet: Boolean)
    extends ConfigParams

object ConfigParamsProtocol extends DefaultJsonProtocol {
  val addrTypeKey = "addrType"
  val isCompressedKey = "isCompressed"
  val isPrivKeyKey = "isPrivkey"
  val isTestNetKey = "isTestnet"

  implicit object ConfigParamsFormatter extends RootJsonFormat[ConfigParams] {

    override def read(value: JsValue): ConfigParams = {
      val obj = value.asJsObject
      val addrTypeOrPrivKey: Either[String, Boolean] = parseAddrTypeOrPrivKey(
        obj)
      val isPrivKey = obj.fields(isPrivKeyKey).convertTo[Boolean]
      val isTestNet = obj.fields(isTestNetKey).convertTo[Boolean]
      ConfigParamsImpl(addrTypeOrPrivKey, isPrivKey, isTestNet)
    }

    override def write(config: ConfigParams): JsValue = ???

    def parseAddrTypeOrPrivKey(obj: JsObject): Either[String, Boolean] = {
      if (obj.fields.contains("addrType")) {
        Left(obj.fields(addrTypeKey).convertTo[String])
      } else Right(obj.fields(isCompressedKey).convertTo[Boolean])
    }
  }
}
