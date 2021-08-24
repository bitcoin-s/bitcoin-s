package org.bitcoins.db.util

import org.bitcoins.core.api.keymanager.MasterXPubApi
import org.bitcoins.db.DbAppConfig
import org.bitcoins.db.models.MasterXPubDAO
import org.bitcoins.keymanager.WalletStorage

import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Future}

/** A trait for checking invariants on file based seeds against a database stored master xpub */
trait DBMasterXPubApi extends MasterXPubApi { dbAppConfig: DbAppConfig =>

  def seedPath: Path

  override def seedExists()(implicit ec: ExecutionContext): Future[Boolean] = {
    val masterXPubDAO: MasterXPubDAO = MasterXPubDAO()(ec, dbAppConfig)
    val fileExists = WalletStorage.seedExists(seedPath)
    masterXPubDAO.existsOneXpub().map { bool =>
      bool && fileExists
    }
  }
}
