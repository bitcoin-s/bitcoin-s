package org.bitcoins.testkit.wallet

import org.bitcoins.testkit.Implicits._
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  RegTestNetChainParams
}
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.wallet.models.AccountDb
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.script.ScriptWitness
import org.bitcoins.core.protocol.script.P2WPKHWitnessV0
import org.bitcoins.wallet.models.LegacySpendingInfo
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.wallet.models.SegwitV0SpendingInfo
import org.bitcoins.testkit.core.gen.NumberGenerator
import org.scalacheck.Gen
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.testkit.fixtures.WalletDAOs
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.bitcoins.wallet.models.SegWitAddressDb
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.wallet.models.AddressDb

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

  private def freshXpub(): ExtPublicKey =
    CryptoGenerators.extPublicKey.sampleSome

  val firstAccount = HDAccount(HDCoin(HDPurposes.SegWit, hdCoinType), 0)
  def firstAccountDb = AccountDb(freshXpub(), firstAccount)

  private def randomScriptWitness: ScriptWitness =
    P2WPKHWitnessV0(freshXpub().key)

  private def randomTXID = CryptoGenerators.doubleSha256Digest.sampleSome.flip
  private def randomVout = NumberGenerator.uInt32s.sampleSome

  /** Between 0 and 10 confirmations */
  private def randomConfs: Int = Gen.choose(0, 10).sampleSome

  private def randomSpent: Boolean = math.random > 0.5

  def sampleSegwitUTXO(spk: ScriptPubKey): SegwitV0SpendingInfo = {
    val outpoint = TransactionOutPoint(randomTXID, randomVout)
    val output =
      TransactionOutput(1.bitcoin, spk)
    val scriptWitness = randomScriptWitness
    val privkeyPath = WalletTestUtil.sampleSegwitPath
    SegwitV0SpendingInfo(confirmations = randomConfs,
                         spent = randomSpent,
                         txid = randomTXID,
                         outPoint = outpoint,
                         output = output,
                         privKeyPath = privkeyPath,
                         scriptWitness = scriptWitness)
  }

  def sampleLegacyUTXO(spk: ScriptPubKey): LegacySpendingInfo = {
    val outpoint =
      TransactionOutPoint(randomTXID, randomVout)
    val output =
      TransactionOutput(1.bitcoin, spk)
    val privKeyPath = WalletTestUtil.sampleLegacyPath
    LegacySpendingInfo(confirmations = randomConfs,
                       spent = randomSpent,
                       txid = randomTXID,
                       outPoint = outpoint,
                       output = output,
                       privKeyPath = privKeyPath)
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
}
