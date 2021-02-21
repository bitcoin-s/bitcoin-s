package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.ScriptPubKeyDb
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.script.ScriptType
import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.wallet.config.WalletAppConfig
import slick.dbio.DBIOAction

import scala.concurrent.{ExecutionContext, Future}

case class ScriptPubKeyDAO()(implicit
    ec: ExecutionContext,
    config: WalletAppConfig
) extends CRUDAutoInc[ScriptPubKeyDb] {

  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: profile.api.TableQuery[ScriptPubKeyTable] =
    TableQuery[ScriptPubKeyTable]

  /** Creates a new row in the database only if the given SPK (not ID) does not exists. */
  def createIfNotExists(spkDb: ScriptPubKeyDb): Future[ScriptPubKeyDb] = {
    val spkFind = table.filter(_.scriptPubKey === spkDb.scriptPubKey).result
    val actions = for {
      spkOpt <- spkFind.headOption
      spk <- spkOpt match {
        case Some(foundSpk) =>
          DBIOAction.successful(foundSpk)
        case None =>
          for {
            newSpkId <- (table returning table.map(_.id)) += spkDb
          } yield {
            spkDb.copyWithId(newSpkId)
          }
      }
    } yield spk

    safeDatabase.run(actions.transactionally)
  }

  /** Finds a scriptPubKey in the database, if it exists */
  def findScriptPubKey(spk: ScriptPubKey): Future[Option[ScriptPubKeyDb]] = {
    val foundVecF = findScriptPubKeys(Vector(spk))
    foundVecF.map(_.headOption)
  }

  /** Searches for the given set of spks and returns the ones that exist in the db */
  def findScriptPubKeys(
      spks: Vector[ScriptPubKey]): Future[Vector[ScriptPubKeyDb]] = {
    val query = table.filter(_.scriptPubKey.inSet(spks))
    safeDatabase.runVec(query.result)
  }

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
        Some(
          (scriptPubKeyDb.id,
           scriptPubKeyDb.scriptPubKey,
           scriptPubKeyDb.scriptPubKey.scriptType))
    }

    override def * =
      (id.?, scriptPubKey, scriptType).<>(fromTuple, toTuple)
  }

}
