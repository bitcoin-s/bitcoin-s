package org.bitcoins.testkit.wallet

import org.bitcoins.testkit.Implicits._
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  RegTestNetChainParams
}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.wallet.models.AccountDb
import scodec.bits.HexStringSyntax
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.script.ScriptWitness
import org.bitcoins.core.protocol.script.P2WPKHWitnessV0
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.testkit.core.gen.TransactionGenerators
import scala.concurrent.Future
import org.bitcoins.wallet.models.AddressDb
import org.bitcoins.wallet.models.AddressDbHelper
import org.bitcoins.testkit.fixtures.WalletDAOs
import scala.concurrent.ExecutionContext
import org.bitcoins.wallet.models.IncomingWalletTXO
import org.bitcoins.wallet.models.LegacySpendingInfo
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.wallet.models.SegwitV0SpendingInfo
import org.bitcoins.wallet.models.SpendingInfoDb

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

  def freshXpub(): ExtPublicKey =
    CryptoGenerators.extPublicKey.sampleSome

  val firstAccount = HDAccount(HDCoin(HDPurposes.SegWit, hdCoinType), 0)
  def firstAccountDb = AccountDb(freshXpub(), firstAccount)

  lazy val sampleTxid: DoubleSha256Digest = DoubleSha256Digest(
    hex"a910523c0b6752fbcb9c24303b4e068c505825d074a45d1c787122efb4649215")
  lazy val sampleVout: UInt32 = UInt32.zero
  lazy val sampleSPK: ScriptPubKey =
    ScriptPubKey.fromAsmBytes(hex"001401b2ac67587e4b603bb3ad709a8102c30113892d")

  lazy val sampleScriptWitness: ScriptWitness = P2WPKHWitnessV0(freshXpub().key)

  lazy val sampleSegwitUTXO: SegwitV0SpendingInfo = {
    val outpoint =
      TransactionOutPoint(WalletTestUtil.sampleTxid, WalletTestUtil.sampleVout)
    val output = TransactionOutput(1.bitcoin, WalletTestUtil.sampleSPK)
    val scriptWitness = WalletTestUtil.sampleScriptWitness
    val privkeyPath = WalletTestUtil.sampleSegwitPath
    SegwitV0SpendingInfo(outPoint = outpoint,
                         output = output,
                         privKeyPath = privkeyPath,
                         scriptWitness = scriptWitness)
  }

  lazy val sampleLegacyUTXO: LegacySpendingInfo = {
    val outpoint =
      TransactionOutPoint(WalletTestUtil.sampleTxid, WalletTestUtil.sampleVout)
    val output = TransactionOutput(1.bitcoin, WalletTestUtil.sampleSPK)
    val privKeyPath = WalletTestUtil.sampleLegacyPath
    LegacySpendingInfo(outPoint = outpoint,
                       output = output,
                       privKeyPath = privKeyPath)
  }

  /**
    * Inserts a incoming TXO, and returns it with the address it was sent to
    *
    * This method also does some asserts on the result, to make sure what
    * we're writing and reading matches up
    */
  def insertIncomingTxo(daos: WalletDAOs, utxo: SpendingInfoDb)(
      implicit ec: ExecutionContext): Future[(IncomingWalletTXO, AddressDb)] = {

    require(utxo.id.isDefined)

    val WalletDAOs(accountDAO, addressDAO, txoDAO, _, utxoDAO) = daos

    /** Get a TX with outputs */
    def getTx: Transaction =
      TransactionGenerators.transaction
        .suchThat(_.outputs.nonEmpty)
        .sample
        .getOrElse(getTx)

    val account = WalletTestUtil.firstAccountDb

    val address = {
      val pub = ECPublicKey()
      val path =
        account.hdAccount
          .toChain(HDChainType.External)
          .toHDAddress(0)
          .toPath

      AddressDbHelper.getAddress(pub, path, RegTest)
    }

    val tx = getTx
    val txoDb = IncomingWalletTXO(confirmations = 3,
                                  txid = tx.txIdBE,
                                  spent = false,
                                  scriptPubKey = address.scriptPubKey,
                                  spendingInfoID = utxo.id.get)
    for {
      _ <- accountDAO.create(account)
      _ <- addressDAO.create(address)
      _ <- utxoDAO.create(utxo)
      createdTxo <- txoDAO.create(txoDb)
      txAndAddrs <- txoDAO.withAddress(createdTxo.txid)
    } yield
      txAndAddrs match {
        case Vector() =>
          throw new org.scalatest.exceptions.TestFailedException(
            s"Couldn't read back TX with address from DB!",
            0)
        case ((foundTxo, foundAddr)) +: _ =>
          assert(foundTxo.confirmations == txoDb.confirmations)
          assert(foundTxo.scriptPubKey == txoDb.scriptPubKey)
          assert(foundTxo.txid == txoDb.txid)

          assert(foundAddr == address)

          (foundTxo, foundAddr)
      }
  }
}
