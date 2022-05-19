package org.bitcoins.lnurl

import org.bitcoins.core.number._
import org.bitcoins.core.util._
import org.bitcoins.crypto.StringFactory
import scodec.bits.ByteVector

import java.net.URL
import scala.util.{Failure, Success, Try}

class LnURL private (private val str: String) {

  val url: URL = LnURL.decode(str) match {
    case Failure(_) =>
      throw new IllegalArgumentException("Invalid LnURL encoding")
    case Success(value) => new URL(value)
  }

  override def toString: String = str.toUpperCase
}

object LnURL extends StringFactory[LnURL] {
  final val lnurlHRP = "lnurl"

  def decode(l: LnURL): Try[String] = decode(l.url)

  def decode(url: URL): Try[String] = decode(url.toString)

  def decode(url: String): Try[String] = {
    Bech32.splitToHrpAndData(url, Bech32Encoding.Bech32).map {
      case (hrp, data) =>
        require(hrp.equalsIgnoreCase(lnurlHRP),
                s"LNURL must start with $lnurlHRP")
        val converted = NumberUtil.convertUInt5sToUInt8(data)
        val bytes = UInt8.toBytes(converted)
        new String(bytes.toArray, "UTF-8")
    }
  }

  override def fromStringT(string: String): Try[LnURL] = {
    LnURL.decode(string).map(fromURL)
  }

  override def fromString(string: String): LnURL = {
    fromStringT(string).get
  }

  def fromURL(uri: String): LnURL = {
    val bytes = ByteVector(uri.getBytes)
    val data = NumberUtil.convertUInt8sToUInt5s(UInt8.toUInt8s(bytes))
    val dataWithHRP = Bech32.hrpExpand(lnurlHRP) ++ data
    val checksum = Bech32.createChecksum(dataWithHRP, Bech32Encoding.Bech32)
    val all: Vector[UInt5] = data ++ checksum
    val encoding = Bech32.encode5bitToString(all)

    new LnURL(lnurlHRP + Bech32.separator + encoding)
  }

}
