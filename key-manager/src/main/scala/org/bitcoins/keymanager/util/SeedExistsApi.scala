package org.bitcoins.keymanager.util

import org.bitcoins.db.DbAppConfig
import org.bitcoins.db.models.MasterXPubDAO

import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.keymanager.WalletStorage

trait SeedExistsApi {

  /** Determines if the seed exists */
  def seedExists()(implicit ec: ExecutionContext): Future[Boolean]
}

/** A trait for checking invariants on file based seeds */
trait FileBasedSeedExists extends SeedExistsApi { dbAppConfig: DbAppConfig =>

  def seedPath: Path

  override def seedExists()(implicit ec: ExecutionContext): Future[Boolean] = {
    val masterXPubDAO: MasterXPubDAO = MasterXPubDAO()(ec, dbAppConfig)
    val fileExists = WalletStorage.seedExists(seedPath)
    masterXPubDAO.existsOneXpub().map { bool =>
      bool && fileExists
    }
  }
}
