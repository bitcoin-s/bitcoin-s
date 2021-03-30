package org.bitcoins.explorer.picklers

import org.bitcoins.commons.serializers.JsonReaders
import org.bitcoins.explorer.model.ExplorerEvent
import play.api.libs.json.Json

object ExplorerPicklers {
  import JsonReaders._
  implicit val explorerEventRW = Json.reads[ExplorerEvent]
}
