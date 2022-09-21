package org.bitcoins.dlc.commons.oracle

import org.bitcoins.core.dlc.oracle.{
  NonceSignaturePairDb,
  OracleMetadataDb,
  OracleMetadataDbHelper,
  OracleMetadataWithId
}
import org.bitcoins.core.protocol.tlv.OracleMetadata
import org.bitcoins.crypto.{SchnorrNonce, SchnorrPublicKey}
import slick.dbio.{DBIOAction, Effect, NoStream}

import scala.concurrent.{ExecutionContext, Future}

trait OracleCommonDataManagement {

  protected val oracleMetadataDAO: OracleMetadataDAO
  protected val oracleSchnorrNonceDAO: OracleSchnorrNonceDAO

  private lazy val safeDatabase = oracleMetadataDAO.safeDatabase

  def getOracleMetadataAction(id: Long)(implicit
      ec: ExecutionContext): DBIOAction[
    Option[OracleMetadataWithId],
    NoStream,
    Effect.Read] = {
    val action = for {
      metadataDbOpt <- oracleMetadataDAO.findByPrimaryKeyAction(id)
      nonceSignaturesDbs <- oracleSchnorrNonceDAO.findByIdAction(id)
    } yield {
      metadataDbOpt.map { metadataDb =>
        OracleMetadata.fromDbs(metadataDb, nonceSignaturesDbs)
      }
    }
    action
  }

  def getOracleMetadata(id: Long)(implicit
      ec: ExecutionContext): Future[Option[OracleMetadataWithId]] = {
    val action = getOracleMetadataAction(id)
    safeDatabase.run(action)
  }

  def createOracleMetaDataAction(oracleMetaData: OracleMetadata)(implicit
      ec: ExecutionContext): DBIOAction[
    (OracleMetadataDb, Vector[NonceSignaturePairDb]),
    NoStream,
    Effect.Write] = {
    val metdataDb = OracleMetadataDbHelper.fromOracleMetadata(oracleMetaData)

    val createMetaDataA = oracleMetadataDAO.createAction(metdataDb)

    val combinedA = for {
      metadataDbWithId <- createMetaDataA
      id = metadataDbWithId.id.get
      nonceSigPairs = oracleMetaData.nonceSignatures
      withIds = nonceSigPairs.map(p =>
        NonceSignaturePairDb(id, p.nonce, p.nonceSignature))
      nonceSignatureDbs <- oracleSchnorrNonceDAO.createAllAction(withIds)
    } yield (metadataDbWithId, nonceSignatureDbs)

    combinedA
  }

  def createOracleMetadata(oracleMetaData: OracleMetadata)(implicit
      ec: ExecutionContext): Future[
    (OracleMetadataDb, Vector[NonceSignaturePairDb])] = {
    val action = createOracleMetaDataAction(oracleMetaData)
    safeDatabase.run(action)
  }

  def findMetadataByAttestationPubKeyAction(
      attestationPubKey: SchnorrPublicKey)(implicit
      ec: ExecutionContext): DBIOAction[
    Vector[OracleMetadataWithId],
    NoStream,
    Effect.Read] = {
    val metadataOptA =
      oracleMetadataDAO.findByAttestationPubKeyAction(attestationPubKey)
    for {
      metadataDbs <- metadataOptA
      metdataActionVec = {
        metadataDbs.map { metadata =>
          oracleSchnorrNonceDAO
            .findByIdAction(metadata.id.get)
            .map(nonces => OracleMetadata.fromDbs(metadata, nonces))
        }
      }
      metadataVec <- DBIOAction.sequence(metdataActionVec)
    } yield metadataVec
  }

  def findMetadataByAttestationPubKey(attestationPubKey: SchnorrPublicKey)(
      implicit ec: ExecutionContext): Future[Vector[OracleMetadataWithId]] = {
    val action = findMetadataByAttestationPubKeyAction(attestationPubKey)
    safeDatabase.run(action)
  }

  def findMetadataByNonce(nonce: SchnorrNonce)(implicit
      ec: ExecutionContext): Future[Option[OracleMetadataWithId]] = {

    for {
      nonceOpt <- oracleSchnorrNonceDAO.findByNonce(nonce)
      metadataOpt <- {
        nonceOpt match {
          case Some(db) => getOracleMetadata(db.id)
          case None     => Future.successful(None)
        }
      }
    } yield metadataOpt

  }
}
