package org.bitcoins.testkit.wallet

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.gcs.BlockFilter
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.AppConfig
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.bitcoins.wallet.config.WalletAppConfig
import org.scalatest.AsyncTestSuite

import scala.concurrent.Future

/** Base test trait for all the tests in our walletTest module */
trait BaseWalletTest extends EmbeddedPg { _: AsyncTestSuite =>

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(getFreshConfig.walletConf)
    super[EmbeddedPg].beforeAll()
  }

  override def afterAll(): Unit = {
    super[EmbeddedPg].afterAll()
  }

  val legacyWalletConf: Config =
    ConfigFactory.parseString("bitcoin-s.wallet.defaultAccountType = legacy")

  val segwitWalletConf: Config =
    ConfigFactory.parseString("bitcoin-s.wallet.defaultAccountType = segwit")

  // This is a random block on testnet
  val testBlockHash: DoubleSha256DigestBE = DoubleSha256DigestBE.fromHex(
    "00000000496dcc754fabd97f3e2df0a7337eab417d75537fecf97a7ebb0e7c75")

  /** Wallet config with data directory set to user temp directory */
  implicit protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl)

  implicit protected def getFreshWalletAppConfig: WalletAppConfig = {
    getFreshConfig.walletConf
  }

  def getBIP39PasswordOpt(): Option[String] =
    KeyManagerTestUtil.bip39PasswordOpt

  def chainQueryApi: ChainQueryApi =
    new ChainQueryApi {

      /** Gets the height of the given block */
      override def getBlockHeight(
          blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
        if (blockHash == testBlockHash)
          Future.successful(Some(1))
        else FutureUtil.none

      /** Gets the hash of the block that is what we consider "best" */
      override def getBestBlockHash(): Future[DoubleSha256DigestBE] =
        Future.successful(testBlockHash)

      /** Gets number of confirmations for the given block hash */
      override def getNumberOfConfirmations(
          blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
        if (blockHash == testBlockHash)
          Future.successful(Some(6))
        else FutureUtil.none

      /** Gets the number of compact filters in the database */
      override def getFilterCount(): Future[Int] = Future.successful(1)

      /** Returns the block height of the given block stamp */
      override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
        Future.successful(1)

      override def getFiltersBetweenHeights(
          startHeight: Int,
          endHeight: Int): Future[Vector[FilterResponse]] =
        Future.successful {
          import scodec.bits._

          // This is a filter for the random block on testnet
          val filterBytes: ByteVector =
            hex"fd2701f0ed169ad16107a8a74609b9e4de3c6133c564f79923ca228805d3" ++
              hex"8e3efc796c4b35034cb573b10b759cdda5efd19e1cdb4d343afcb06455fa" ++
              hex"820b06eca828ad61d3377fa464f3bd06ff4432310a363f667e13d09ba993" ++
              hex"264c703a0aa668b33eaa555bd3e93ac85dfde380ab723aafd407dfa13ffe" ++
              hex"2e7ddf6f452bd0d977617c4ab2dc3b38c26810023984ad57890e3cf34cfc" ++
              hex"2d4a6973b9430ede26bfd9f5bb24e043d48483d84b9025d0a940b15f13fc" ++
              hex"0a1e77abd7626869f417c7710e9a6315477691d7c4e2c50f0e776755a62a" ++
              hex"b6f0e8eb7a3be8d1a8c3d9dd4602efc5146f0d431d1669378d7afa03c7b9" ++
              hex"84d9b0b78007abb6e7c036156e5186d1d79a2f37daecfcbe8821cf42851c" ++
              hex"b10ef0c359307d54e53078eb631f02c067a474dceb484da20bc0e7c5451a" ++
              hex"b957f46b306caa82938b19bb34fd76c5cc07e048932524704dec8f72c91c" ++
              hex"d5ee1f4648de839047a0bea0d4d4d66c19cfccc2b5f285a84af18114f608" ++
              hex"f144391648aedfb5ffcccbb51272512d6ba9a2e19a47cebe5b50a8a7073a" ++
              hex"1c24059440444047a41bdbab16f61bc4b0ee8987de82fd25cc62abc86e2b" ++
              hex"577fc55175be138680df7253a8bcae9d9954391d3bed806ce5a6869b4553" ++
              hex"0f214486b1b7f0347efcfde58ca0882f059f7b1541c74506930897c78e23" ++
              hex"a6c94b49856369606ed652b8c7402a49f289cb5d1098bb999112225327e0" ++
              hex"a32efd2bcd192a2ffbd1997c6a3b7d1a9445bc31fb57485ebe0c431e482b" ++
              hex"04e509e557cff107cee08a45c22aa3cbdcb9d305bd95c919e90239e0ec29" ++
              hex"2a5418a6151f431e8ab82278b3d816ecd483f43d3d657dae9996cc523fdd" ++
              hex"242c4e01935db91a2936e9398ff7278b8a3430eed99ad25fc2a41afc0b4a" ++
              hex"e417f6c1785414607cfa13f04173740333a5b58655c74a51deddb38cf8c3" ++
              hex"d50b7d2ccf380cad34a5c341e7155494cc4560dff3b19bf88b4d73e9ce76" ++
              hex"cbeff573fe93674e4a752d06d5321ff00a4582d62683fb4986d36eaec825" ++
              hex"c14d41b2d5aefaf539e989f7fa097eac657c70b975c56e26b73fb9401ce3" ++
              hex"81502f0883d52c6a3bcc956e0ea1787f0717d0205fecfe55b01edb1ac0"
          Vector(
            FilterResponse(compactFilter = BlockFilter
                             .fromBytes(filterBytes, testBlockHash.flip),
                           blockHash = testBlockHash,
                           blockHeight = 1))
        }

      override def epochSecondToBlockHeight(time: Long): Future[Int] =
        Future.successful(0)
    }

}
