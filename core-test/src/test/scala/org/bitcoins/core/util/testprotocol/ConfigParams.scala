package org.bitcoins.core.util.testprotocol

import ujson._
import upickle.default._

case class ConfigParams(
    addrTypeOrIsCompressed: Either[String, Boolean],
    isPrivKey: Boolean,
    isTestNet: Boolean)

object ConfigParams {
  val addrTypeKey = "addrType"
  val isCompressedKey = "isCompressed"
  val isPrivKeyKey = "isPrivkey"
  val isTestNetKey = "isTestnet"

  implicit val configParamsR: Reader[ConfigParams] = reader[Value].map {
    value =>
      val obj = value.obj
      val addrTypeOrPrivKey: Either[String, Boolean] =
        parseAddrTypeOrPrivKey(obj)
      val isPrivKey = obj(isPrivKeyKey).bool
      val isTestNet = obj(isTestNetKey).bool

      ConfigParams(addrTypeOrPrivKey, isPrivKey, isTestNet)
  }

  def parseAddrTypeOrPrivKey(obj: Obj): Either[String, Boolean] = {
    if (obj.value.contains("addrType")) {
      Left(obj(addrTypeKey).str)
    } else Right(obj(isCompressedKey).bool)
  }
}
