package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.wallet.config._

import java.sql.SQLException
import scala.concurrent.{ExecutionContext, Future}

case class SpendingInfoDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends CRUDAutoInc[UTXORecord] {
  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  /** The table inside our database we are inserting into */
  override val table: profile.api.TableQuery[SpendingInfoTable] =
    profile.api.TableQuery[SpendingInfoTable]

  private lazy val addrTable: profile.api.TableQuery[
    AddressDAO#AddressTable] = {
    AddressDAO()(ec, appConfig).table
  }

  private lazy val txTable: profile.api.TableQuery[
    IncomingTransactionDAO#IncomingTransactionTable] = {
    IncomingTransactionDAO().table
  }

  private lazy val tagTable: profile.api.TableQuery[
    AddressTagDAO#AddressTagTable] = {
    AddressTagDAO().table
  }

  private lazy val spkTable: profile.api.TableQuery[
    ScriptPubKeyDAO#ScriptPubKeyTable] = {
    ScriptPubKeyDAO().table
  }

  def create(si: SpendingInfoDb): Future[SpendingInfoDb] = {
    val query =
      table.returning(table.map(_.id)).into((t, id) => t.copyWithId(id = id))

    val actions = for {
      utxo: UTXORecord <- insertAction(si, query)
      spk <-
        spkTable
          .filter(_.id === utxo.scriptPubKeyId)
          .result
          .headOption
    } yield (utxo, spk)

    safeDatabase
      .run(actions)
      .map {
        case (utxo, Some(spk)) => utxo.toSpendingInfoDb(spk.scriptPubKey)
        case _ =>
          throw new SQLException(
            s"Unexpected result: Cannot create either a UTXO or a SPK record for $si")
      }
  }

  def createUnless(si: SpendingInfoDb)(
      condition: (UTXORecord, UTXORecord) => Boolean): Future[
    SpendingInfoDb] = {
    val query =
      table.returning(table.map(_.id)).into((t, id) => t.copyWithId(id = id))

    val actions = for {
      foundOpt <- table.filter(_.outPoint === si.outPoint).result.headOption
      cond <- foundOpt match {
        case Some(foundUtxo) =>
          val utxoToCreate =
            UTXORecord.fromSpendingInfoDb(si, foundUtxo.scriptPubKeyId)
          DBIO.successful(condition(foundUtxo, utxoToCreate))
        case None => DBIO.successful(false)
      }
      utxo <-
        if (cond) DBIO.successful(foundOpt.get) else insertAction(si, query)
      spk <-
        spkTable
          .filter(_.id === utxo.scriptPubKeyId)
          .result
          .headOption
    } yield (utxo, spk)

    safeDatabase
      .run(actions)
      .map {
        case (utxo, Some(spk)) => utxo.toSpendingInfoDb(spk.scriptPubKey)
        case _ =>
          throw new SQLException(
            s"Unexpected result: Cannot create either a UTXO or a SPK record for $si")
      }
  }

  private def insertAction(
      si: SpendingInfoDb,
      query: profile.IntoInsertActionComposer[
        UTXORecord,
        UTXORecord]): DBIOAction[
    UTXORecord,
    NoStream,
    Effect.Read with Effect.Write] = {
    for {
      spkOpt <-
        spkTable
          .filter(_.scriptPubKey === si.output.scriptPubKey)
          .result
          .headOption
      utxo: UTXORecord <- spkOpt match {
        case Some(foundSpk) =>
          val utxo = UTXORecord.fromSpendingInfoDb(si, foundSpk.id.get)
          query += utxo
        case None =>
          (for {
            newSpkId <-
              (spkTable returning spkTable.map(_.id)) += (ScriptPubKeyDb(
                si.output.scriptPubKey))
          } yield {
            val utxo = UTXORecord.fromSpendingInfoDb(si, newSpkId)
            query += utxo
          }).flatten
      }
    } yield utxo
  }

  def upsertAllSpendingInfoDb(
      ts: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    FutureUtil.foldLeftAsync(Vector.empty[SpendingInfoDb], ts)((acc, si) =>
      upsert(si).map(res => acc :+ res))
  }

  def updateAllSpendingInfoDb(
      ts: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    FutureUtil.foldLeftAsync(Vector.empty[SpendingInfoDb], ts)((acc, si) =>
      update(si).map(res => acc :+ res))
  }

  def update(si: SpendingInfoDb): Future[SpendingInfoDb] = {
    val actions = for {
      spkOpt <-
        spkTable
          .filter(_.scriptPubKey === si.output.scriptPubKey)
          .result
          .headOption
      _ <- spkOpt match {
        case Some(foundSpk) =>
          val utxo = UTXORecord.fromSpendingInfoDb(si, foundSpk.id.get)
          table.filter(_.id === utxo.id).update(utxo)
        case None =>
          (for {
            newSpkId <-
              (spkTable returning spkTable.map(_.id)) += ScriptPubKeyDb(
                si.output.scriptPubKey)
          } yield {
            val utxo = UTXORecord.fromSpendingInfoDb(si, newSpkId)
            table.filter(_.id === utxo.id).update(utxo)
          }).flatten
      }
      utxo <- table.filter(_.id === si.id.get).result.headOption
      spk <-
        spkTable
          .filter(_.scriptPubKey === si.output.scriptPubKey)
          .result
          .headOption
    } yield (utxo, spk)
    safeDatabase
      .run(actions)
      .map {
        case (Some(utxo), Some(spk)) => utxo.toSpendingInfoDb(spk.scriptPubKey)
        case _ =>
          throw new SQLException(
            s"Unexpected result: Cannot update either a UTXO or a SPK record for $si")
      }
  }

  def upsert(si: SpendingInfoDb): Future[SpendingInfoDb] = {
    val actions = for {
      spkOpt <-
        spkTable
          .filter(_.scriptPubKey === si.output.scriptPubKey)
          .result
          .headOption
      _ <- spkOpt match {
        case Some(foundSpk) =>
          table.insertOrUpdate(
            UTXORecord.fromSpendingInfoDb(si, foundSpk.id.get))
        case None =>
          (for {
            newSpkId <-
              (spkTable returning spkTable.map(_.id)) += ScriptPubKeyDb(
                si.output.scriptPubKey)
          } yield table.insertOrUpdate(
            UTXORecord.fromSpendingInfoDb(si, newSpkId))).flatten
      }
      utxo <- table.filter(_.outPoint === si.outPoint).result.headOption
      spk <-
        spkTable
          .filter(_.scriptPubKey === si.output.scriptPubKey)
          .result
          .headOption
    } yield (utxo, spk)
    safeDatabase
      .run(actions)
      .map {
        case (Some(utxo), Some(spk)) => utxo.toSpendingInfoDb(spk.scriptPubKey)
        case _ =>
          throw new SQLException(
            s"Unexpected result: Cannot upsert either a UTXO or a SPK record for $si")
      }
  }

  def delete(si: SpendingInfoDb): Future[Int] = {
    val action = deleteAction(si)
    safeDatabase.run(action)
  }

  def deleteAction(
      si: SpendingInfoDb): DBIOAction[Int, NoStream, Effect.Write] = {
    deleteSpendingInfoDbAllAction(Vector(si))
  }

  def deleteSpendingInfoDbAllAction(
      sis: Vector[SpendingInfoDb]): DBIOAction[Int, NoStream, Effect.Write] = {
    val ids = sis.map(_.id).flatten
    val query = table.filter(t => t.id.inSet(ids))
    query.delete
  }

  def findAllSpendingInfosAction(): DBIOAction[
    Vector[SpendingInfoDb],
    NoStream,
    Effect.Read] = {
    for {
      all <- findAllAction()
      utxos <- utxoToInfoAction(all)
    } yield utxos
  }

  def findAllSpendingInfos(): Future[Vector[SpendingInfoDb]] =
    for {
      all <- findAll()
      utxos <- utxoToInfo(all)
    } yield utxos

  /** Fetches all the received TXOs in our DB that are in
    * the given TX
    */
  def findTx(tx: Transaction): Future[Vector[SpendingInfoDb]] =
    findTxs(Vector(tx))

  /** Fetches all received txos in our db that are in the given txs */
  def findTxs(txs: Vector[Transaction]): Future[Vector[SpendingInfoDb]] = {
    findOutputsReceived(txs.map(_.txIdBE))
  }

  private def _findOutputsBeingSpentQuery(
      txs: Vector[Transaction]): Query[SpendingInfoTable, UTXORecord, Seq] = {
    val outPoints = txs
      .flatMap(_.inputs)
      .map(_.previousOutput)

    val filtered = table
      .filter { case txo =>
        txo.outPoint.inSet(outPoints)
      }
    filtered
  }

  /** Finds all the outputs being spent in the given
    * transaction
    */
  def findOutputsBeingSpent(tx: Transaction): Future[Vector[SpendingInfoDb]] = {
    findOutputsBeingSpent(Vector(tx))
  }

  private def findOutputsBeingSpentQuery(txs: Vector[Transaction]): Query[
    (SpendingInfoTable, ScriptPubKeyDAO#ScriptPubKeyTable),
    (UTXORecord, ScriptPubKeyDAO#ScriptPubKeyTable#TableElementType),
    Seq] = {
    _findOutputsBeingSpentQuery(txs)
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
  }

  def findOutputsBeingSpent(
      txs: Vector[Transaction]): Future[Vector[SpendingInfoDb]] = {
    val action: DBIOAction[
      Vector[(UTXORecord, ScriptPubKeyDb)],
      NoStream,
      Effect.Read] = findOutputsBeingSpentQuery(txs).result
      .map(_.toVector)

    val resultsF = safeDatabase.runVec(action)

    for {
      results <- resultsF
    } yield {
      results.map { case (utxo, spk) =>
        utxo.toSpendingInfoDb(spk.scriptPubKey)
      }
    }
  }

  /** Given a TXID, fetches all incoming TXOs and the address the TXO pays to
    */
  def withAddress(txid: DoubleSha256DigestBE): Future[
    Vector[(SpendingInfoDb, AddressDb)]] = {
    def _withAddress: Future[Vector[(UTXORecord, AddressRecord)]] = {
      val query = {
        val filtered = table.filter(_.txid === txid)
        filtered.join(addrTable).on(_.scriptPubKeyId === _.scriptPubKeyId)
      }

      safeDatabase.runVec(query.result)
    }

    for {
      res <- _withAddress
      utxoSpks <- findScriptPubKeysByUtxos(res.map(_._1))
      addrSpks <- findScriptPubKeys(res.map(_._2.scriptPubKeyId))
    } yield {
      res.map(r =>
        (r._1.toSpendingInfoDb(utxoSpks(r._1.scriptPubKeyId).scriptPubKey),
         r._2.toAddressDb(addrSpks(r._2.scriptPubKeyId).scriptPubKey)))
    }

  }

  /** Fetches all the incoming TXOs in our DB that are in
    * the transaction with the given TXID
    */
  def findDbsForTx(txid: DoubleSha256DigestBE): Future[Vector[UTXORecord]] = {
    val query = table.filter(_.txid === txid)
    safeDatabase.runVec(query.result)
  }

  /** Joins the spk table on the spending info table with the spk id */
  private val spkJoinQuery = table
    .join(spkTable)
    .on(_.scriptPubKeyId === _.id)

  /** Fetches all the incoming TXOs in our DB that are in
    * the transaction with the given TXID
    */
  def findOutputsReceived(
      txids: Vector[DoubleSha256DigestBE]): Future[Vector[SpendingInfoDb]] = {
    val filtered = spkJoinQuery
      .filter(_._1.state.inSet(TxoState.receivedStates))
      .filter(_._1.txid.inSet(txids))
    safeDatabase
      .runVec(filtered.result)
      .map(res =>
        res.map { case (utxoRec, spkRec) =>
          utxoRec.toSpendingInfoDb(spkRec.scriptPubKey)
        })
  }

  def findByScriptPubKey(
      scriptPubKey: ScriptPubKey): Future[Vector[SpendingInfoDb]] = {
    val filtered = spkJoinQuery.filter(_._2.scriptPubKey === scriptPubKey)
    safeDatabase
      .runVec(filtered.result)
      .map(res =>
        res.map { case (utxoRec, spkRec) =>
          utxoRec.toSpendingInfoDb(spkRec.scriptPubKey)
        })
  }

  def findByScriptPubKeyId(scriptPubKeyId: Long): Future[Vector[UTXORecord]] = {
    val filtered = table.filter(_.scriptPubKeyId === scriptPubKeyId)
    safeDatabase.runVec(filtered.result)
  }

  /** Enumerates all unspent TX outputs in the wallet with the state
    * [[TxoState.PendingConfirmationsReceived]] or [[TxoState.ConfirmedReceived]]
    */
  def _findAllUnspent(): Future[Vector[UTXORecord]] = {
    val query = table.filter(_.state.inSet(TxoState.receivedStates))

    safeDatabase.run(query.result).map(_.toVector)
  }

  def utxoToInfoAction(utxos: Vector[UTXORecord]): DBIOAction[
    Vector[SpendingInfoDb],
    NoStream,
    Effect.Read] = {
    for {
      spks <- findScriptPubKeysAction(utxos)
    } yield utxos.map(utxo =>
      utxo.toSpendingInfoDb(spks(utxo.scriptPubKeyId).scriptPubKey))
  }

  def utxoToInfo(utxos: Vector[UTXORecord]): Future[Vector[SpendingInfoDb]] =
    for {
      spks <- findScriptPubKeysByUtxos(utxos)
    } yield utxos.map(utxo =>
      utxo.toSpendingInfoDb(spks(utxo.scriptPubKeyId).scriptPubKey))

  def infoToUtxo(infos: Vector[SpendingInfoDb]): Future[Vector[UTXORecord]] =
    for {
      spks <- findPublicKeyScriptsBySpendingInfoDb(infos)
    } yield infos.map(utxo =>
      UTXORecord.fromSpendingInfoDb(utxo, spks(utxo.output.scriptPubKey)))

  def findAllUnspent(): Future[Vector[SpendingInfoDb]] = {
    for {
      utxos <- _findAllUnspent()
      infos <- utxoToInfo(utxos)
    } yield infos
  }

  private def filterUtxosByAccount(
      utxos: Vector[SpendingInfoDb],
      hdAccount: HDAccount): Vector[SpendingInfoDb] = {
    utxos.filter(utxo =>
      HDAccount.isSameAccount(bip32Path = utxo.privKeyPath,
                              account = hdAccount))
  }

  /** Finds all utxos for a given account */
  def findAllUnspentForAccount(
      hdAccount: HDAccount): Future[Vector[SpendingInfoDb]] = {
    val allUtxosF = findAllUnspent()
    allUtxosF.map(filterUtxosByAccount(_, hdAccount))
  }

  def findAllForAccountAction(hdAccount: HDAccount): DBIOAction[
    Vector[SpendingInfoDb],
    NoStream,
    Effect.Read] = {
    findAllSpendingInfosAction().map(filterUtxosByAccount(_, hdAccount))
  }

  def findAllForAccount(
      hdAccount: HDAccount): Future[Vector[SpendingInfoDb]] = {
    val action = findAllForAccountAction(hdAccount)
    safeDatabase.run(action)
  }

  def findByTxoState(state: TxoState): Future[Vector[SpendingInfoDb]] = {
    val query = table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
    val filtered = query.filter(_._1.state === state)

    safeDatabase
      .runVec(filtered.result)
      .map(res =>
        res.map { case (utxoRec, spkRec) =>
          utxoRec.toSpendingInfoDb(spkRec.scriptPubKey)
        })
  }

  /** Enumerates all TX outputs in the wallet with the state
    * [[TxoState.PendingConfirmationsReceived]] or [[TxoState.PendingConfirmationsSpent]]
    */
  def findAllPendingConfirmation: Future[Vector[SpendingInfoDb]] = {
    val query = table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
    val filtered = query.filter(_._1.state.inSet(TxoState.pendingConfStates))

    safeDatabase
      .runVec(filtered.result)
      .map(res =>
        res.map { case (utxoRec, spkRec) =>
          utxoRec.toSpendingInfoDb(spkRec.scriptPubKey)
        })
  }

  /** Enumerates all TX outputs in the wallet with the state
    * [[TxoState.BroadcastSpent]] or [[TxoState.BroadcastReceived]]
    */
  def findAllInMempool: Future[Vector[SpendingInfoDb]] = {
    val query = table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
    val filtered = query.filter(_._1.state.inSet(TxoState.broadcastStates))

    safeDatabase
      .runVec(filtered.result)
      .map(res =>
        res.map { case (utxoRec, spkRec) =>
          utxoRec.toSpendingInfoDb(spkRec.scriptPubKey)
        })
  }

  /** Enumerates all TX outpoints in the wallet */
  def findAllOutpoints(): Future[Vector[TransactionOutPoint]] = {
    val query = table.map(_.outPoint)
    safeDatabase.runVec(query.result).map(_.toVector)
  }

  def findByOutPoint(
      outPoint: TransactionOutPoint): Future[Option[SpendingInfoDb]] = {
    findByOutPoints(Vector(outPoint)).map(_.headOption)
  }

  /** Enumerates all TX outpoints in the wallet */
  def findByOutPoints(outPoints: Vector[TransactionOutPoint]): Future[
    Vector[SpendingInfoDb]] = {
    val query = table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
    val filtered = query.filter(_._1.outPoint.inSet(outPoints))
    safeDatabase
      .runVec(filtered.result)
      .map(res =>
        res.map { case (utxoRec, spkRec) =>
          utxoRec.toSpendingInfoDb(spkRec.scriptPubKey)
        })
  }

  def findAllUnspentForTag(tag: AddressTag): Future[Vector[SpendingInfoDb]] = {
    val query = table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
      .filter(_._1.state.inSet(TxoState.receivedStates))
      .join(addrTable)
      .on(_._1.scriptPubKeyId === _.scriptPubKeyId)
      .join(tagTable)
      .on(_._2.address === _.address)
      .filter(_._2.tagName === tag.tagName)
      .filter(_._2.tagType === tag.tagType)

    safeDatabase
      .runVec(query.result)
      .map(_.map { case (((utxoRecord, spkDb), _), _) =>
        utxoRecord.toSpendingInfoDb(spkDb.scriptPubKey)
      })
  }

  def markAsReserved(
      ts: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    //1. Check if any are reserved already
    //2. if not, reserve them
    //3. if they are reserved, throw an exception?
    val outPoints = ts.map(_.outPoint)
    val action: DBIOAction[
      Int,
      NoStream,
      Effect.Write with Effect.Transactional] = table
      .filter(_.outPoint.inSet(outPoints))
      .filter(
        _.state.inSet(TxoState.receivedStates)
      ) //must be available to reserve
      .map(_.state)
      .update(TxoState.Reserved)
      .flatMap { count =>
        if (count != ts.length) {
          val exn = new RuntimeException(
            s"Failed to reserve all utxos, expected=${ts.length} actual=$count")
          DBIO.failed(exn)
        } else {

          DBIO.successful(count)
        }
      }

    safeDatabase
      .run(action)
      .map(_ => ts.map(_.copyWithState(TxoState.Reserved)))
  }

  private def findScriptPubKeysAction(ids: Seq[Long]): DBIOAction[
    Map[Long, ScriptPubKeyDb],
    NoStream,
    Effect.Read] = {
    val query = spkTable.filter(t => t.id.inSet(ids))
    query.result.map { action =>
      action.map { case spk =>
        (spk.id.get, spk)
      }.toMap
    }
  }

  private def findScriptPubKeys(
      ids: Seq[Long]): Future[Map[Long, ScriptPubKeyDb]] = {
    val action = findScriptPubKeysAction(ids)
    safeDatabase.run(action)
  }

  private def findScriptPubKeysAction(utxos: Vector[UTXORecord]): DBIOAction[
    Map[Long, ScriptPubKeyDb],
    NoStream,
    Effect.Read] = {
    val ids = utxos.map(_.scriptPubKeyId)
    findScriptPubKeysAction(ids)
  }

  private def findScriptPubKeysByUtxos(
      utxos: Vector[UTXORecord]): Future[Map[Long, ScriptPubKeyDb]] = {
    val action = findScriptPubKeysAction(utxos)
    safeDatabase.run(action)
  }

  private def findPublicKeyScriptsBySpendingInfoDb(
      spendingInfoDbs: Seq[SpendingInfoDb]): Future[Map[ScriptPubKey, Long]] = {
    val spks = spendingInfoDbs.map(_.output.scriptPubKey)
    val query = spkTable.filter(t => t.scriptPubKey.inSet(spks))
    safeDatabase
      .runVec(query.result)
      .map(_.map(spk => (spk.scriptPubKey, spk.id.get)).toMap)
  }

  /** This table stores the necessary information to spend
    * a transaction output (TXO) at a later point in time. It
    * also stores how many confirmations it has, whether
    * or not it is spent (i.e. if it is a UTXO or not) and the
    * TXID of the transaction that created this output.
    */
  case class SpendingInfoTable(tag: Tag)
      extends TableAutoInc[UTXORecord](tag, schemaName, "txo_spending_info") {

    def outPoint: Rep[TransactionOutPoint] =
      column("tx_outpoint", O.Unique)

    def txid: Rep[DoubleSha256DigestBE] = column("txid")

    def state: Rep[TxoState] = column("txo_state")

    def scriptPubKeyId: Rep[Long] = column("script_pub_key_id")

    def value: Rep[CurrencyUnit] = column("value")

    def privKeyPath: Rep[HDPath] = column("hd_privkey_path")

    def redeemScriptOpt: Rep[Option[ScriptPubKey]] =
      column("redeem_script")

    def scriptWitnessOpt: Rep[Option[ScriptWitness]] = column("script_witness")

    def spendingTxIdOpt: Rep[Option[DoubleSha256DigestBE]] = column(
      "spending_txid")

    /** All UTXOs must have a SPK in the wallet that gets spent to */
    def fk_scriptPubKeyId: slick.lifted.ForeignKeyQuery[_, ScriptPubKeyDb] = {
      val scriptPubKeyTable = spkTable
      foreignKey("fk_scriptPubKeyId",
                 sourceColumns = scriptPubKeyId,
                 targetTableQuery = scriptPubKeyTable)(_.id)
    }

    /** All UTXOs must have a corresponding transaction in the wallet */
    def fk_incoming_txId: slick.lifted.ForeignKeyQuery[
      _,
      IncomingTransactionDb] = {
      foreignKey("fk_incoming_txId",
                 sourceColumns = txid,
                 targetTableQuery = txTable)(_.txIdBE)
    }

    private val fromTuple: (
        (
            TransactionOutPoint,
            DoubleSha256DigestBE,
            TxoState,
            Long,
            CurrencyUnit,
            HDPath,
            Option[ScriptPubKey],
            Option[ScriptWitness],
            Option[DoubleSha256DigestBE],
            Option[Long])) => UTXORecord = {
      case (outpoint,
            _,
            state,
            scriptPubKeyId,
            value,
            path,
            redeemScriptOpt,
            scriptWitOpt,
            spendingTxIdOpt,
            idOpt) =>
        UTXORecord(outpoint,
                   state,
                   scriptPubKeyId,
                   value,
                   path,
                   redeemScriptOpt,
                   scriptWitOpt,
                   spendingTxIdOpt,
                   idOpt)
    }

    private val toTuple: UTXORecord => Option[
      (
          TransactionOutPoint,
          DoubleSha256DigestBE,
          TxoState,
          Long,
          CurrencyUnit,
          HDPath,
          Option[ScriptPubKey],
          Option[ScriptWitness],
          Option[DoubleSha256DigestBE],
          Option[Long])] = { case utxo: UTXORecord =>
      Some(
        (utxo.outpoint,
         utxo.outpoint.txIdBE,
         utxo.state,
         utxo.scriptPubKeyId,
         utxo.value,
         utxo.path,
         utxo.redeemScript,
         utxo.scriptWitness,
         utxo.spendingTxIdOpt,
         utxo.id))
    }

    override def * = {
      (outPoint,
       txid,
       state,
       scriptPubKeyId,
       value,
       privKeyPath,
       redeemScriptOpt,
       scriptWitnessOpt,
       spendingTxIdOpt,
       id.?).<>(fromTuple, toTuple)
    }
  }
}
