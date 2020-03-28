package org.bitcoins.testkit.wallet

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  RegTestNetChainParams
}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.{Bech32Address, P2SHAddress}
import org.bitcoins.core.util.{CryptoUtil, NumberUtil}
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator}
import org.bitcoins.testkit.fixtures.WalletDAOs
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models._

import scala.concurrent.{ExecutionContext, Future}

object WalletTestUtil {

  val chainParams: ChainParams = RegTestNetChainParams
  val networkParam: RegTest.type = RegTest

  val hdCoinType: HDCoinType = HDCoinType.Testnet

  /**
    * Useful if you want wallet test runs
    * To use the same key values each time
    */
  val sampleMnemonic =
    MnemonicCode.fromWords(
      Vector("portion",
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
             "car"))

  lazy val sampleSegwitPath =
    SegWitHDPath(hdCoinType,
                 accountIndex = 0,
                 HDChainType.External,
                 addressIndex = 0)

  /** Sample legacy HD path */
  lazy val sampleLegacyPath = LegacyHDPath(hdCoinType,
                                           accountIndex = 0,
                                           HDChainType.Change,
                                           addressIndex = 0)

  lazy val sampleNestedSegwitPath: NestedSegWitHDPath =
    NestedSegWitHDPath(hdCoinType,
                       accountIndex = 0,
                       HDChainType.External,
                       addressIndex = 0)

  private def freshXpub(): ExtPublicKey =
    CryptoGenerators.extPublicKey.sampleSome

  val defaultHdAccount = HDAccount(HDCoin(HDPurposes.SegWit, hdCoinType), 0)

  def getHdAccount1(walletAppConfig: WalletAppConfig): HDAccount = {
    val purpose = walletAppConfig.defaultAccountKind
    HDAccount(coin = HDCoin(purpose, HDCoinType.Testnet), index = 1)
  }

  def firstAccountDb = AccountDb(freshXpub(), defaultHdAccount)

  def nestedSegWitAccountDb: AccountDb =
    AccountDb(freshXpub(),
              HDAccount(HDCoin(HDPurposes.NestedSegWit, hdCoinType), 0))

  private def randomScriptWitness: ScriptWitness =
    P2WPKHWitnessV0(freshXpub().key)

  private def randomTXID = CryptoGenerators.doubleSha256Digest.sampleSome.flip
  private def randomVout = NumberGenerator.uInt32s.sampleSome
  private def randomBlockHash =
    CryptoGenerators.doubleSha256Digest.sampleSome.flip

  private def randomState: TxoState = {
    val idx = NumberUtil.posInt % TxoState.all.length
    TxoState.all(idx)
  }

  def sampleSegwitUTXO(spk: ScriptPubKey): SegwitV0SpendingInfo = {
    val outpoint = TransactionOutPoint(randomTXID, randomVout)
    val output =
      TransactionOutput(1.bitcoin, spk)
    val scriptWitness = randomScriptWitness
    val privkeyPath = WalletTestUtil.sampleSegwitPath
    SegwitV0SpendingInfo(
      state = randomState,
      txid = randomTXID,
      outPoint = outpoint,
      output = output,
      privKeyPath = privkeyPath,
      scriptWitness = scriptWitness,
      blockHash = Some(randomBlockHash)
    )
  }

  def sampleLegacyUTXO(spk: ScriptPubKey): LegacySpendingInfo = {
    val outpoint =
      TransactionOutPoint(randomTXID, randomVout)
    val output =
      TransactionOutput(1.bitcoin, spk)
    val privKeyPath = WalletTestUtil.sampleLegacyPath
    LegacySpendingInfo(state = randomState,
                       txid = randomTXID,
                       outPoint = outpoint,
                       output = output,
                       privKeyPath = privKeyPath,
                       blockHash = Some(randomBlockHash))
  }

  def sampleNestedSegwitUTXO(
      ecPublicKey: ECPublicKey): NestedSegwitV0SpendingInfo = {
    val wpkh = P2WPKHWitnessSPKV0(ecPublicKey)
    val outpoint = TransactionOutPoint(randomTXID, randomVout)
    val output =
      TransactionOutput(1.bitcoin, P2SHScriptPubKey(wpkh))
    val scriptWitness = randomScriptWitness
    val privkeyPath = WalletTestUtil.sampleNestedSegwitPath
    NestedSegwitV0SpendingInfo(
      state = randomState,
      txid = randomTXID,
      outPoint = outpoint,
      output = output,
      privKeyPath = privkeyPath,
      redeemScript = wpkh,
      scriptWitness = scriptWitness,
      blockHash = Some(randomBlockHash)
    )
  }

  /** Given an account returns a sample address */
  def getAddressDb(account: AccountDb): AddressDb = {
    val path = SegWitHDPath(WalletTestUtil.hdCoinType,
                            chainType = HDChainType.External,
                            accountIndex = account.hdAccount.index,
                            addressIndex = 0)
    val pubkey: ECPublicKey = ECPublicKey.freshPublicKey
    val hashedPubkey = CryptoUtil.sha256Hash160(pubkey.bytes)
    val wspk = P2WPKHWitnessSPKV0(pubkey)
    val scriptWitness = P2WPKHWitnessV0(pubkey)
    val address = Bech32Address.apply(wspk, WalletTestUtil.networkParam)

    SegWitAddressDb(path = path,
                    ecPublicKey = pubkey,
                    hashedPubkey,
                    address,
                    scriptWitness,
                    scriptPubKey = wspk)
  }

  /** Given an account returns a sample address */
  def getNestedSegwitAddressDb(account: AccountDb): AddressDb = {
    val path = NestedSegWitHDPath(WalletTestUtil.hdCoinType,
                                  chainType = HDChainType.External,
                                  accountIndex = account.hdAccount.index,
                                  addressIndex = 0)
    val pubkey: ECPublicKey = ECPublicKey.freshPublicKey
    val hashedPubkey = CryptoUtil.sha256Hash160(pubkey.bytes)
    val wpkh = P2WPKHWitnessSPKV0(pubkey)
    val witness = P2WPKHWitnessV0(pubkey)
    val spk = P2SHScriptPubKey(wpkh)
    val address = P2SHAddress.apply(spk, WalletTestUtil.networkParam)

    NestedSegWitAddressDb(path = path,
                          ecPublicKey = pubkey,
                          hashedPubkey,
                          address,
                          witness,
                          scriptPubKey = spk)
  }

  /** Inserts an account, address and finally a UTXO */
  def insertLegacyUTXO(daos: WalletDAOs)(
      implicit ec: ExecutionContext): Future[LegacySpendingInfo] = {
    for {
      account <- daos.accountDAO.create(WalletTestUtil.firstAccountDb)
      addr <- daos.addressDAO.create(getAddressDb(account))
      utxo <- daos.utxoDAO.create(sampleLegacyUTXO(addr.scriptPubKey))
    } yield utxo.asInstanceOf[LegacySpendingInfo]
  }

  /** Inserts an account, address and finally a UTXO */
  def insertSegWitUTXO(daos: WalletDAOs)(
      implicit ec: ExecutionContext): Future[SegwitV0SpendingInfo] = {
    for {
      account <- daos.accountDAO.create(WalletTestUtil.firstAccountDb)
      addr <- daos.addressDAO.create(getAddressDb(account))
      utxo <- daos.utxoDAO.create(sampleSegwitUTXO(addr.scriptPubKey))
    } yield utxo.asInstanceOf[SegwitV0SpendingInfo]
  }

  /** Inserts an account, address and finally a UTXO */
  def insertNestedSegWitUTXO(daos: WalletDAOs)(
      implicit ec: ExecutionContext): Future[NestedSegwitV0SpendingInfo] = {
    for {
      account <- daos.accountDAO.create(WalletTestUtil.nestedSegWitAccountDb)
      addr <- daos.addressDAO.create(getNestedSegwitAddressDb(account))
      utxo <- daos.utxoDAO.create(sampleNestedSegwitUTXO(addr.ecPublicKey))
    } yield utxo.asInstanceOf[NestedSegwitV0SpendingInfo]
  }
}
