package org.bitcoins.core.api.wallet.db

import org.bitcoins.core.api.db.DbRowAutoInc
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.crypto.{CryptoUtil, Sha256Digest}

case class ScriptPubKeyDb(
    scriptPubKey: ScriptPubKey,
    hash: Sha256Digest,
    id: Option[Long] = None)
    extends DbRowAutoInc[ScriptPubKeyDb] {
  override def copyWithId(id: Long): ScriptPubKeyDb = copy(id = Option(id))
}

object ScriptPubKeyDb {

  def apply(scriptPubKey: ScriptPubKey): ScriptPubKeyDb =
    ScriptPubKeyDb(
      scriptPubKey = scriptPubKey,
      hash = hash(scriptPubKey)
    )

  def hash(scriptPubKey: ScriptPubKey): Sha256Digest =
    CryptoUtil.sha256(scriptPubKey.bytes)
}
