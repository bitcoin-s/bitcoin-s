package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.ScriptPubKeyDb
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.script.ScriptType
import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.ExecutionContext

case class ScriptPubKeyDAO()(implicit
    ec: ExecutionContext,
    config: WalletAppConfig
) extends CRUDAutoInc[ScriptPubKeyDb] {

  import profile.api._

  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)

  import mappers._

  override val table: profile.api.TableQuery[ScriptPubKeyTable] =
    TableQuery[ScriptPubKeyTable]

  case class ScriptPubKeyTable(tag: Tag)
      extends TableAutoInc[ScriptPubKeyDb](tag, schemaName, "pub_key_scripts") {

    def scriptPubKey: Rep[ScriptPubKey] = column("script_pub_key")
    def scriptType: Rep[ScriptType] = column("script_type")

    private type ScriptPubKeyTuple = (Option[Long], ScriptPubKey, ScriptType)

    private val fromTuple: ScriptPubKeyTuple => ScriptPubKeyDb = {
      case (id, scriptPubKey, scriptType) =>
        require(
          scriptPubKey.scriptType == scriptType,
          s"script type must match it script: `${scriptPubKey.scriptType}` != `${scriptType}` ")
        ScriptPubKeyDb(scriptPubKey, id)

    }

    private val toTuple: ScriptPubKeyDb => Option[ScriptPubKeyTuple] = {
      scriptPubKeyDb =>
        Some(scriptPubKeyDb.id,
             scriptPubKeyDb.scriptPubKey,
             scriptPubKeyDb.scriptPubKey.scriptType)
    }

    override def * =
      (id.?, scriptPubKey, scriptType) <> (fromTuple, toTuple)
  }

}
