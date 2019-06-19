package org.bitcoins.testkit.wallet

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto._
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
import org.bitcoins.wallet.models.IncomingTransaction
import org.bitcoins.wallet.models.AddressDb
import org.bitcoins.wallet.models.AddressDbHelper
import org.bitcoins.testkit.fixtures.WalletDAOs
import scala.concurrent.ExecutionContext

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

  def freshXpub: ExtPublicKey =
    CryptoGenerators.extPublicKey.sample.getOrElse(freshXpub)

  val firstAccount = HDAccount(HDCoin(HDPurposes.SegWit, hdCoinType), 0)
  def firstAccountDb = AccountDb(freshXpub, firstAccount)

  lazy val sampleTxid: DoubleSha256Digest = DoubleSha256Digest(
    hex"a910523c0b6752fbcb9c24303b4e068c505825d074a45d1c787122efb4649215")
  lazy val sampleVout: UInt32 = UInt32.zero
  lazy val sampleSPK: ScriptPubKey =
    ScriptPubKey.fromAsmBytes(hex"001401b2ac67587e4b603bb3ad709a8102c30113892d")

  lazy val sampleScriptWitness: ScriptWitness = P2WPKHWitnessV0(freshXpub.key)

  /**
    * Inserts a incoming TX, and returns it with the address it was sent to
    *
    * This method also does some asserts on the result, to make sure what
    * we're writing and reading matches up
    */
  def insertIncomingTx(daos: WalletDAOs)(implicit ec: ExecutionContext): Future[
    (IncomingTransaction, AddressDb)] = {
    val WalletDAOs(accountDAO, addressDAO, txDAO, _, _) = daos

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
          .toAddress(0)
          .toPath

      AddressDbHelper.getAddress(pub, path, RegTest)
    }

    val tx = getTx
    val txDb = IncomingTransaction(tx,
                                   confirmations = 3,
                                   scriptPubKey = address.scriptPubKey,
                                   voutIndex = 0)
    for {
      _ <- accountDAO.create(account)
      _ <- addressDAO.create(address)
      createdTx <- txDAO.create(txDb)
      txAndAddr <- txDAO.withAddress(createdTx.transaction)
    } yield
      txAndAddr match {
        case None =>
          throw new org.scalatest.exceptions.TestFailedException(
            s"Couldn't read back TX with address from DB!",
            0)
        case Some((foundTx, foundAddr)) =>
          assert(foundTx.confirmations == txDb.confirmations)
          assert(foundTx.scriptPubKey == txDb.scriptPubKey)
          assert(foundTx.transaction == txDb.transaction)

          assert(foundAddr == address)

          (foundTx, foundAddr)
      }

  }
}
