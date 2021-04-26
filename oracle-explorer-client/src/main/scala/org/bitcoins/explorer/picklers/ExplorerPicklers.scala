package org.bitcoins.explorer.picklers

import org.bitcoins.explorer.model._
import org.bitcoins.commons.serializers.JsonReaders._
import play.api.libs.json.{Json, Reads}

object ExplorerPicklers {

  implicit val explorerEventRW: Reads[SbAnnouncementEvent] =
    Json.reads[SbAnnouncementEvent]
  implicit val oracleRW: Reads[Oracle] = Json.reads[Oracle]
}
