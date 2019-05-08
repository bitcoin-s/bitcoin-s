package org.bitcoins.wallet.models

import org.bitcoins.wallet.EncryptedMnemonic
import slick.dbio.DBIOAction
import slick.dbio.Effect.Write
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.TableQuery

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.wallet.config.WalletAppConfig

/**
  * @note This DAO does not extend [[org.bitcoins.db.CRUD CRUD]]
  *       because it's not the intention to create multiple mnenonics
  *
  */
// todo: implement this as a flat file, not a table
case class MnemonicCodeDAO()(implicit executionContext: ExecutionContext) {
  val ec: ExecutionContext = executionContext

  val appConfig = WalletAppConfig
  val database = appConfig.database

  /** The table inside our database we are inserting into */
  val table: TableQuery[MnemonicCodeTable] =
    TableQuery[MnemonicCodeTable]

  /**
    * Any given wallet can only have one mnemonic code
    *
    * todo: error handling, make sure only one mnemonic is present at any given time
    */
  def create(mnemonic: EncryptedMnemonic): Future[EncryptedMnemonic] = {
    val action: DBIOAction[EncryptedMnemonic, NoStream, Write] =
      (table += mnemonic).andThen(DBIOAction.successful(mnemonic))
    database.run(action)
  }

  // todo: implement
  def updatePassphrase(
      oldPassphrase: String,
      newPassphrase: EncryptedMnemonic): Future[EncryptedMnemonic] = {
    ???
  }

  // todo: error handling
  def read(): Future[Option[EncryptedMnemonic]] = {
    val query = table.result.map(_.headOption)
    database.run(query)
  }
}
