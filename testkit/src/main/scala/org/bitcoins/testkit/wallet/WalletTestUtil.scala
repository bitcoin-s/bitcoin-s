package org.bitcoins.testkit.wallet

import org.bitcoins.core.api.wallet.db.*
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.*
import org.bitcoins.core.currency.*
import org.bitcoins.core.hd.*
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  RegTestNetChainParams
}
import org.bitcoins.core.protocol.script.*
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.protocol.{Bech32Address, P2SHAddress}
import org.bitcoins.core.wallet.utxo.*
import org.bitcoins.crypto.{CryptoUtil, DoubleSha256DigestBE, ECPublicKey}
import org.bitcoins.testkitcore.Implicits.*
import org.bitcoins.testkitcore.gen.{CryptoGenerators, NumberGenerator}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.WalletDAOs

import scala.concurrent.{ExecutionContext, Future}

object WalletTestUtil {

  val chainParams: ChainParams = RegTestNetChainParams
  val networkParam: RegTest.type = RegTest

  val hdCoinType: HDCoinType = HDCoinType.Testnet

  lazy val sampleTransaction: Transaction = Transaction(
    "020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000"
  )

  /** Useful if you want wallet test runs To use the same key values each time
    */
  lazy val sampleMnemonic =
    MnemonicCode.fromWords(
      Vector(
        "portion",
        "uniform",
        "owner",
        "crime",
        "duty",
        "floor",
        "sketch",
        "stumble",
        "outer",
        "south",
        "relax",
        "car"
      )
    )

  lazy val sampleSegwitPath =
    SegWitHDPath(
      hdCoinType,
      accountIndex = 0,
      HDChainType.External,
      addressIndex = 0
    )

  /** Sample legacy HD path */
  lazy val sampleLegacyPath = LegacyHDPath(
    hdCoinType,
    accountIndex = 0,
    HDChainType.Change,
    addressIndex = 0
  )

  lazy val sampleNestedSegwitPath: NestedSegWitHDPath =
    NestedSegWitHDPath(
      hdCoinType,
      accountIndex = 0,
      HDChainType.External,
      addressIndex = 0
    )

  private def freshXpub(): ExtPublicKey =
    CryptoGenerators.extPublicKey.sampleSome

  /** Checks that the given values are the same-ish, save for fee-level
    * deviations
    */
  def isCloseEnough(
      first: CurrencyUnit,
      second: CurrencyUnit,
      delta: CurrencyUnit = 300.sats
  ): Boolean = {
    Math.abs(
      first.satoshis.toLong - second.satoshis.toLong
    ) <= delta.satoshis.toLong
  }

  def isFeeRateCloseEnough(outgoingTx: OutgoingTransactionDb): Boolean = {
    TxUtil
      .isValidFeeRange(
        outgoingTx.expectedFee,
        outgoingTx.actualFee,
        outgoingTx.feeRate
      )
      .isSuccess
  }

  val defaultHdAccount: HDAccount =
    HDAccount(HDCoin(HDPurpose.SegWit, hdCoinType), 0)

  def getHdAccount1(walletAppConfig: WalletAppConfig): HDAccount = {
    val purpose = walletAppConfig.defaultPurpose
    HDAccount(coin = HDCoin(purpose, HDCoinType.Testnet), index = 1)
  }

  def firstAccountDb: AccountDb =
    AccountDb(freshXpub(), defaultHdAccount)

  def nestedSegWitAccountDb: AccountDb =
    AccountDb(
      freshXpub(),
      HDAccount(HDCoin(HDPurpose.NestedSegWit, hdCoinType), 0)
    )

  private def randomScriptWitness: ScriptWitness =
    P2WPKHWitnessV0(freshXpub().key)

  def randomTXID: DoubleSha256DigestBE =
    CryptoGenerators.doubleSha256Digest.sampleSome.flip
  private def randomVout = NumberGenerator.uInt32s.sampleSome

  private def randomBlockHash =
    CryptoGenerators.doubleSha256Digest.sampleSome.flip

  def sampleSegwitUTXO(
      spk: ScriptPubKey,
      state: TxoState
  ): SegwitV0SpendingInfo = {
    val outpoint = TransactionOutPoint(randomTXID, randomVout)
    val output =
      TransactionOutput(1.bitcoin, spk)
    val scriptWitness = randomScriptWitness
    val privkeyPath = WalletTestUtil.sampleSegwitPath

    val spendingTxIdOpt = {
      if (TxoState.spentStates.contains(state)) {
        Some(randomTXID)
      } else {
        None
      }
    }

    SegwitV0SpendingInfo(
      state = state,
      outPoint = outpoint,
      output = output,
      privKeyPath = privkeyPath,
      scriptWitness = scriptWitness,
      spendingTxIdOpt = spendingTxIdOpt
    )
  }

  def sampleLegacyUTXO(
      spk: ScriptPubKey,
      state: TxoState
  ): LegacySpendingInfo = {
    val outpoint =
      TransactionOutPoint(randomTXID, randomVout)
    val output =
      TransactionOutput(1.bitcoin, spk)
    val privKeyPath = WalletTestUtil.sampleLegacyPath
    val spendingTxIdOpt = {
      if (TxoState.spentStates.contains(state)) {
        Some(randomTXID)
      } else {
        None
      }
    }

    LegacySpendingInfo(
      state = state,
      outPoint = outpoint,
      output = output,
      privKeyPath = privKeyPath,
      spendingTxIdOpt = spendingTxIdOpt
    )
  }

  def sampleNestedSegwitUTXO(
      ecPublicKey: ECPublicKey,
      state: TxoState
  ): NestedSegwitV0SpendingInfo = {
    val wpkh = P2WPKHWitnessSPKV0(ecPublicKey)
    val outpoint = TransactionOutPoint(randomTXID, randomVout)
    val output =
      TransactionOutput(1.bitcoin, P2SHScriptPubKey(wpkh))
    val scriptWitness = randomScriptWitness
    val privkeyPath = WalletTestUtil.sampleNestedSegwitPath

    val spendingTxIdOpt = {
      if (TxoState.spentStates.contains(state)) {
        Some(randomTXID)
      } else {
        None
      }
    }

    NestedSegwitV0SpendingInfo(
      state = state,
      outPoint = outpoint,
      output = output,
      privKeyPath = privkeyPath,
      redeemScript = wpkh,
      scriptWitness = scriptWitness,
      spendingTxIdOpt = spendingTxIdOpt
    )
  }

  /** Given an account returns a sample address */
  def getAddressDb(account: AccountDb, addressIndex: Int = 0): AddressDb = {
    val path = SegWitHDPath(
      WalletTestUtil.hdCoinType,
      chainType = HDChainType.External,
      accountIndex = account.hdAccount.index,
      addressIndex = addressIndex
    )
    val pubkey: ECPublicKey = ECPublicKey.freshPublicKey
    val hashedPubkey = CryptoUtil.sha256Hash160(pubkey.bytes)
    val wspk = P2WPKHWitnessSPKV0(pubkey)
    val scriptWitness = P2WPKHWitnessV0(pubkey)
    val address = Bech32Address.apply(wspk, WalletTestUtil.networkParam)

    SegWitAddressDb(
      path = path,
      ecPublicKey = pubkey,
      hashedPubkey,
      address,
      scriptWitness,
      scriptPubKey = wspk
    )
  }

  def insertDummyIncomingTransaction(
      daos: WalletDAOs,
      utxo: SpendingInfoDb,
      confirmed: Boolean = true
  )(implicit ec: ExecutionContext): Future[IncomingTransactionDb] = {
    val blockHashOpt = if (confirmed) Some(randomBlockHash) else None

    val txDb = TransactionDb(
      txIdBE = utxo.txid,
      transaction = EmptyTransaction,
      unsignedTxIdBE = utxo.txid,
      unsignedTx = EmptyTransaction,
      wTxIdBEOpt = None,
      totalOutput = Satoshis.zero,
      numInputs = 1,
      numOutputs = 1,
      lockTime = UInt32.zero,
      blockHashOpt = blockHashOpt
    )
    val incomingDb = IncomingTransactionDb(utxo.txid, utxo.output.value)
    for {
      _ <- daos.transactionDAO.upsert(txDb)
      written <- daos.incomingTxDAO.upsert(incomingDb)
    } yield written
  }

  /** Given an account returns a sample address */
  def getNestedSegwitAddressDb(account: AccountDb): AddressDb = {
    val path = NestedSegWitHDPath(
      WalletTestUtil.hdCoinType,
      chainType = HDChainType.External,
      accountIndex = account.hdAccount.index,
      addressIndex = 0
    )
    val pubkey: ECPublicKey = ECPublicKey.freshPublicKey
    val hashedPubkey = CryptoUtil.sha256Hash160(pubkey.bytes)
    val wpkh = P2WPKHWitnessSPKV0(pubkey)
    val witness = P2WPKHWitnessV0(pubkey)
    val spk = P2SHScriptPubKey(wpkh)
    val address = P2SHAddress.apply(spk, WalletTestUtil.networkParam)

    NestedSegWitAddressDb(
      path = path,
      ecPublicKey = pubkey,
      hashedPubkey,
      address,
      witness,
      scriptPubKey = spk
    )
  }

  /** Inserts an account, address and finally a UTXO */
  def insertLegacyUTXO(daos: WalletDAOs, state: TxoState)(implicit
      ec: ExecutionContext
  ): Future[LegacySpendingInfo] = {
    for {
      account <- daos.accountDAO.upsert(WalletTestUtil.firstAccountDb)
      addr <- daos.addressDAO.create(getAddressDb(account))
      utxo = sampleLegacyUTXO(addr.scriptPubKey, state)
      _ <- insertDummyIncomingTransaction(daos, utxo)
      utxoDb <- daos.utxoDAO.create(utxo)
    } yield utxoDb.asInstanceOf[LegacySpendingInfo]
  }

  /** Inserts an account, address and finally a UTXO */
  def insertSegWitUTXO(daos: WalletDAOs, state: TxoState)(implicit
      ec: ExecutionContext
  ): Future[SegwitV0SpendingInfo] = {
    for {
      account <- daos.accountDAO.create(WalletTestUtil.firstAccountDb)
      addr <- daos.addressDAO.create(getAddressDb(account))
      utxo = sampleSegwitUTXO(addr.scriptPubKey, state)
      _ <- insertDummyIncomingTransaction(daos, utxo)
      utxoDb <- daos.utxoDAO.create(utxo)
    } yield utxoDb.asInstanceOf[SegwitV0SpendingInfo]
  }

  /** Inserts an account, address and finally a UTXO */
  def insertNestedSegWitUTXO(daos: WalletDAOs, state: TxoState)(implicit
      ec: ExecutionContext
  ): Future[NestedSegwitV0SpendingInfo] = {
    for {
      account <- daos.accountDAO.create(WalletTestUtil.nestedSegWitAccountDb)
      addr <- daos.addressDAO.create(getNestedSegwitAddressDb(account))
      utxo = sampleNestedSegwitUTXO(addr.ecPublicKey, state)
      _ <- insertDummyIncomingTransaction(daos, utxo)
      utxoDb <- daos.utxoDAO.create(utxo)
    } yield utxoDb.asInstanceOf[NestedSegwitV0SpendingInfo]
  }
}
