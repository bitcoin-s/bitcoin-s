package org.bitcoins.core.api.wallet.db

import org.bitcoins.core.api.db.DbRowAutoInc
import org.bitcoins.core.protocol.script.ScriptPubKey

case class ScriptPubKeyDb(scriptPubKey: ScriptPubKey, id: Option[Long] = None)
    extends DbRowAutoInc[ScriptPubKeyDb] {
  override def copyWithId(id: Long): ScriptPubKeyDb = copy(id = Option(id))
}
