package org.bitcoins.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptWitness,
  WitnessScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.wallet.config._
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class SpendingInfoDAO()(
    implicit val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends CRUDAutoInc[SpendingInfoDb] {
  import org.bitcoins.db.DbCommonsColumnMappers._
  import profile.api._

  /** The table inside our database we are inserting into */
  override val table: profile.api.TableQuery[SpendingInfoTable] =
    profile.api.TableQuery[SpendingInfoTable]

  private lazy val addrTable: profile.api.TableQuery[AddressDAO#AddressTable] = {
    AddressDAO()(ec, appConfig).table
  }

  private lazy val txTable: profile.api.TableQuery[
    IncomingTransactionDAO#IncomingTransactionTable] = {
    IncomingTransactionDAO().table
  }

  /**
    * Fetches all the incoming TXOs in our DB that are in
    * the given TX
    */
  def findTx(tx: Transaction): Future[Vector[SpendingInfoDb]] =
    findTx(tx.txIdBE)

  /**
    * Finds all the outputs being spent in the given
    * transaction
    */
  def findOutputsBeingSpent(tx: Transaction): Future[Seq[SpendingInfoDb]] = {

    val filtered = table
      .filter {
        case txo =>
          txo.outPoint.inSet(tx.inputs.map(_.previousOutput))
      }

    safeDatabase.run(filtered.result)
  }

  /**
    * Given a TXID, fetches all incoming TXOs and the address the TXO pays to
    */
  def withAddress(txid: DoubleSha256DigestBE): Future[
    Vector[(SpendingInfoDb, AddressDb)]] = {
    val query = {
      val filtered = table.filter(_.txid === txid)
      filtered.join(addrTable).on(_.scriptPubKey === _.scriptPubKey)
    }

    safeDatabase.runVec(query.result)
  }

  /** Updates the [[org.bitcoins.core.wallet.utxo.TxoState TxoState]] of all of the given
    * outputs in our database to be the state
    */
  def updateTxoState(
      outputs: Seq[TransactionOutput],
      state: TxoState): Future[Vector[SpendingInfoDb]] = {
    val spks = outputs.map(_.scriptPubKey)
    val filtered = table.filter(_.scriptPubKey.inSet(spks))

    for {
      utxos <- database.run(filtered.result)
      _ = require(
        utxos.length == outputs.length,
        s"Was given ${outputs.length} outputs, found ${utxos.length} in DB")
      newStates = utxos.map(_.copyWithState(state = state)).toVector
      updated <- updateAll(newStates)
    } yield {
      require(utxos.length == updated.length,
              "Updated a different number of UTXOs than what we found!")
      logger.debug(s"Updated ${updated.length} UTXO(s) to state=${state}")
      updated

    }

  }

  /**
    * Fetches all the incoming TXOs in our DB that are in
    * the transaction with the given TXID
    */
  def findTx(txid: DoubleSha256DigestBE): Future[Vector[SpendingInfoDb]] = {
    val filtered = table.filter(_.txid === txid)
    safeDatabase.runVec(filtered.result)
  }

  def findByScriptPubKey(
      scriptPubKey: ScriptPubKey): Future[Vector[SpendingInfoDb]] = {
    val filtered = table.filter(_.scriptPubKey === scriptPubKey)
    safeDatabase.runVec(filtered.result)
  }

  private val receivedStates: Set[TxoState] =
    Set(TxoState.PendingConfirmationsReceived, TxoState.ConfirmedReceived)

  /** Enumerates all unspent TX outputs in the wallet with the state
    * [[TxoState.PendingConfirmationsReceived]] or [[TxoState.ConfirmedReceived]] */
  def findAllUnspent(): Future[Vector[SpendingInfoDb]] = {
    val query = table.filter(_.state.inSet(receivedStates))

    database.run(query.result).map(_.toVector)
  }

  private def filterUtxosByAccount(
      utxos: Vector[SpendingInfoDb],
      hdAccount: HDAccount): Vector[SpendingInfoDb] = {
    utxos.filter(
      utxo =>
        HDAccount.isSameAccount(bip32Path = utxo.privKeyPath,
                                account = hdAccount))
  }

  /** Finds all utxos for a given account */
  def findAllUnspentForAccount(
      hdAccount: HDAccount): Future[Vector[SpendingInfoDb]] = {
    val allUtxosF = findAllUnspent()
    allUtxosF.map(filterUtxosByAccount(_, hdAccount))
  }

  def findAllForAccount(
      hdAccount: HDAccount): Future[Vector[SpendingInfoDb]] = {
    val allUtxosF = findAll()
    allUtxosF.map(filterUtxosByAccount(_, hdAccount))
  }

  /** Enumerates all TX outputs in the wallet with the state
    * [[TxoState.PendingConfirmationsReceived]] or [[TxoState.PendingConfirmationsSpent]] */
  def findAllPendingConfirmation: Future[Vector[SpendingInfoDb]] = {
    val query = table.filter(_.state.inSet(TxoState.pendingConfStates))

    database.run(query.result).map(_.toVector)
  }

  /** Enumerates all TX outpoints in the wallet */
  def findAllOutpoints(): Future[Vector[TransactionOutPoint]] = {
    val query = table.map(_.outPoint)
    safeDatabase.runVec(query.result).map(_.toVector)
  }

  /** Enumerates all TX outpoints in the wallet */
  def findByOutPoints(outPoints: Vector[TransactionOutPoint]): Future[
    Vector[SpendingInfoDb]] = {
    val query = table.filter(_.outPoint.inSet(outPoints))
    safeDatabase.runVec(query.result).map(_.toVector)
  }

  /**
    * This table stores the necessary information to spend
    * a transaction output (TXO) at a later point in time. It
    * also stores how many confirmations it has, whether
    * or not it is spent (i.e. if it is a UTXO or not) and the
    * TXID of the transaction that created this output.
    */
  case class SpendingInfoTable(tag: Tag)
      extends TableAutoInc[SpendingInfoDb](tag, "txo_spending_info") {
    import org.bitcoins.db.DbCommonsColumnMappers._

    def outPoint: Rep[TransactionOutPoint] =
      column("tx_outpoint")

    def txid: Rep[DoubleSha256DigestBE] = column("txid")

    def state: Rep[TxoState] = column("txo_state")

    def scriptPubKey: Rep[ScriptPubKey] = column("script_pub_key")

    def value: Rep[CurrencyUnit] = column("value")

    def privKeyPath: Rep[HDPath] = column("hd_privkey_path")

    def redeemScriptOpt: Rep[Option[ScriptPubKey]] =
      column("redeem_script")

    def scriptWitnessOpt: Rep[Option[ScriptWitness]] = column("script_witness")

    def blockHash: Rep[Option[DoubleSha256DigestBE]] = column("block_hash")

    /** All UTXOs must have a SPK in the wallet that gets spent to */
    def fk_scriptPubKey: slick.lifted.ForeignKeyQuery[_, AddressDb] = {
      val addressTable = addrTable
      foreignKey("fk_scriptPubKey",
                 sourceColumns = scriptPubKey,
                 targetTableQuery = addressTable)(_.scriptPubKey)
    }

    /** All UTXOs must have a corresponding transaction in the wallet */
    def fk_incoming_txId: slick.lifted.ForeignKeyQuery[
      _,
      IncomingTransactionDb] = {
      foreignKey("fk_incoming_txId",
                 sourceColumns = txid,
                 targetTableQuery = txTable)(_.txIdBE)
    }

    private type UTXOTuple = (
        Option[Long], // ID
        TransactionOutPoint,
        ScriptPubKey, // output SPK
        CurrencyUnit, // output value
        HDPath,
        Option[ScriptPubKey], // ReedemScript
        Option[ScriptWitness],
        TxoState, // state
        DoubleSha256DigestBE, // TXID
        Option[DoubleSha256DigestBE] // block hash
    )

    private val fromTuple: UTXOTuple => SpendingInfoDb = {
      case (id,
            outpoint,
            spk,
            value,
            path: SegWitHDPath,
            None, // ReedemScript
            Some(scriptWitness),
            state,
            txid,
            blockHash) =>
        SegwitV0SpendingInfo(
          outPoint = outpoint,
          output = TransactionOutput(value, spk),
          privKeyPath = path,
          scriptWitness = scriptWitness,
          id = id,
          state = state,
          txid = txid,
          blockHash = blockHash
        )

      case (id,
            outpoint,
            spk,
            value,
            path: LegacyHDPath,
            None, // RedeemScript
            None, // ScriptWitness
            state,
            txid,
            blockHash) =>
        LegacySpendingInfo(outPoint = outpoint,
                           output = TransactionOutput(value, spk),
                           privKeyPath = path,
                           id = id,
                           state = state,
                           txid = txid,
                           blockHash = blockHash)

      case (id,
            outpoint,
            spk,
            value,
            path: NestedSegWitHDPath,
            Some(redeemScript), // RedeemScript
            Some(scriptWitness), // ScriptWitness
            state,
            txid,
            blockHash)
          if WitnessScriptPubKey.isWitnessScriptPubKey(redeemScript.asm) =>
        NestedSegwitV0SpendingInfo(outpoint,
                                   TransactionOutput(value, spk),
                                   path,
                                   redeemScript,
                                   scriptWitness,
                                   txid,
                                   state,
                                   blockHash,
                                   id)

      case (id,
            outpoint,
            spk,
            value,
            path,
            spkOpt,
            swOpt,
            spent,
            txid,
            blockHash) =>
        throw new IllegalArgumentException(
          "Could not construct UtxoSpendingInfoDb from bad tuple:"
            + s" ($id, $outpoint, $spk, $value, $path, $spkOpt, $swOpt, $spent, $txid, $blockHash).")
    }

    private val toTuple: SpendingInfoDb => Option[UTXOTuple] =
      utxo =>
        Some(
          (utxo.id,
           utxo.outPoint,
           utxo.output.scriptPubKey,
           utxo.output.value,
           utxo.privKeyPath,
           utxo.redeemScriptOpt,
           utxo.scriptWitnessOpt,
           utxo.state,
           utxo.txid,
           utxo.blockHash))

    def * : ProvenShape[SpendingInfoDb] =
      (id.?,
       outPoint,
       scriptPubKey,
       value,
       privKeyPath,
       redeemScriptOpt,
       scriptWitnessOpt,
       state,
       txid,
       blockHash) <> (fromTuple, toTuple)
  }
}
