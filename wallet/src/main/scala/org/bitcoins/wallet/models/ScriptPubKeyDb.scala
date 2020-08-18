package org.bitcoins.wallet.models

import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.api.db.DbRowAutoInc

case class ScriptPubKeyDb(scriptPubKey: ScriptPubKey, id: Option[Long] = None)
    extends DbRowAutoInc[ScriptPubKeyDb] {
  override def copyWithId(id: Long): ScriptPubKeyDb = copy(id = Option(id))
}
