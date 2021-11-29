package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.db.CRUD

import scala.concurrent.Future

trait DLCIdDaoUtil[T, PrimaryKeyType] { _: CRUD[T, PrimaryKeyType] =>
  import profile.api._

  def findByDLCIdAction(dlcId: Sha256Digest): profile.api.DBIOAction[
    Vector[T],
    profile.api.NoStream,
    profile.api.Effect.Read]

  def findByDLCId(dlcId: Sha256Digest): Future[Vector[T]] = {
    safeDatabase.runVec(findByDLCIdAction(dlcId).transactionally)
  }

  def deleteByDLCIdAction(dlcId: Sha256Digest): profile.api.DBIOAction[
    Int,
    profile.api.NoStream,
    profile.api.Effect.Write]

  def deleteByDLCId(dlcId: Sha256Digest): Future[Int] = {
    safeDatabase.run(deleteByDLCIdAction(dlcId).transactionally)
  }
}
