package org.bitcoins.explorer.model

import org.bitcoins.core.protocol.tlv.OracleAnnouncementV0TLV

case class CreateAnnouncementExplorer(
    oracleAnnouncementV0: OracleAnnouncementV0TLV,
    oracleName: String,
    description: String,
    eventURI: Option[String]) {

  override def toString: String = {
    val base =
      s"oracleAnnouncementV0=${oracleAnnouncementV0.hex}&description=$description&oracleName=$oracleName"
    eventURI match {
      case None => base
      case Some(uri) =>
        val uriString = s"&uri=$uri"
        base + uriString
    }
  }
}
