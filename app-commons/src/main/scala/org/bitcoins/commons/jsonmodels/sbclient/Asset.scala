package org.bitcoins.commons.jsonmodels.sbclient

sealed abstract class Asset {
  def toLowerString: String = this.toString.toLowerCase
  def toUpperString: String = this.toString.toUpperCase
}

object Asset {
  case object BTC extends Asset
  case object ETH extends Asset
  case object USD extends Asset
  case object USDT extends Asset
  case object LTC extends Asset
  case object BCH extends Asset
  case object XRP extends Asset
  case object EOS extends Asset
  case object EUR extends Asset

  case object DOGE extends Asset
  case object STR extends Asset

  case class OtherAsset(name: String) extends Asset {
    override def toString: String = name
  }

  def fromLowerString(str: String): Asset = {
    val name = if (str.toLowerCase == "xbt") "btc" else str
    val assetOpt = all.find(asset => asset.toLowerString == name)
    assetOpt.getOrElse(OtherAsset(name.toUpperCase))
  }

  def fromUpperString(str: String): Asset = {
    val name = if (str.toUpperCase == "XBT") "BTC" else str
    val assetOpt = all.find(asset => asset.toUpperString == name)
    assetOpt.getOrElse(OtherAsset(name))
  }

  def fromString(str: String): Asset = {
    fromUpperString(str.toUpperCase)
  }

  def all: Vector[Asset] =
    Vector(BTC, ETH, USD, USDT, LTC, BCH, XRP, EOS, EUR, DOGE, STR)
}
