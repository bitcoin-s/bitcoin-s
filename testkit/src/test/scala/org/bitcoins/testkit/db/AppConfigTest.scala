package org.bitcoins.testkit.db

import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.testkit.Implicits._
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.BitcoinSAppConfig._
import com.typesafe.config.ConfigFactory
import org.bitcoins.core.config.TestNet3
import org.bitcoins.chain.models.BlockHeaderDAO
import akka.actor.ActorSystem
import scala.concurrent.ExecutionContext
import org.bitcoins.wallet.models.AccountDAO
import org.bitcoins.testkit.chain.ChainTestUtil
import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.chain.models.BlockHeaderDbHelper
import org.bitcoins.wallet.models.AccountDb
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.hd.HDCoin
import org.bitcoins.core.hd.HDPurposes
import org.bitcoins.core.hd.HDCoinType
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.db.DbManagement
import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.db.SQLiteTableInfo
import slick.jdbc.SQLiteProfile.api._
import org.bitcoins.db.CRUD
import java.nio.file.Files
import org.bitcoins.testkit.BitcoinSTestAppConfig

class AppConfigTest extends BitcoinSUnitTest {

  val system = ActorSystem()
  implicit val ec: ExecutionContext = system.dispatcher

  behavior of "BitcoinSAppConfig"

  it must "propagate values correctly to all sub configs" in {
    val networkOverride =
      ConfigFactory.parseString("bitcoin-s.network = testnet3")

    val config = BitcoinSTestAppConfig.getTestConfig(networkOverride)
    val chainConf = config.chainConf
    val walletConf = config.walletConf
    val nodeConf = config.nodeConf

    assert(chainConf.datadir == walletConf.datadir)
    assert(walletConf.datadir == nodeConf.datadir)

    assert(chainConf.network == TestNet3)
    assert(walletConf.network == TestNet3)
    assert(nodeConf.network == TestNet3)
  }

  it must "have the same DB path" in {
    val conf = BitcoinSTestAppConfig.getTestConfig()
    val chainConf = conf.chainConf
    val walletConf = conf.walletConf
    val nodeConf = conf.nodeConf
    assert(chainConf.dbPath == walletConf.dbPath)
    assert(walletConf.dbPath == nodeConf.dbPath)
  }

  it must "have distinct databases" in {
    val conf = BitcoinSTestAppConfig.getTestConfig()
    val chainConf = conf.chainConf
    val walletConf = conf.walletConf
    val nodeConf = conf.nodeConf
    assert(chainConf.dbName != walletConf.dbName)
    assert(walletConf.dbName != nodeConf.dbName)
  }

  it must "be able to write to distinct databases" in {
    implicit val config = BitcoinSTestAppConfig.getTestConfig()
    val chainConf = config.chainConf
    val walletConf = config.walletConf
    val nodeConf = config.nodeConf
    val allConfs = List(chainConf, walletConf, nodeConf)

    val bhDAO = BlockHeaderDAO()
    val accountDAO = AccountDAO()

    allConfs.foreach { conf =>
      val fullDbPath = conf.dbPath.resolve(conf.dbName)
      assert(!Files.exists(fullDbPath))
    }

    val writeF = {
      for {
        _ <- config.initialize()
        _ = {
          allConfs.foreach { conf =>
            val fullDbPath = conf.dbPath.resolve(conf.dbName)
            assert(Files.isRegularFile(fullDbPath))
          }
        }
        _ <- {
          bhDAO.create(ChainTestUtil.regTestGenesisHeaderDb)
        }
        _ <- {
          val hdAccount =
            HDAccount(HDCoin(HDPurposes.Legacy, HDCoinType.Bitcoin), 0)
          val xpub = CryptoGenerators.extPublicKey.sampleSome
          val account = AccountDb(xpub, hdAccount)
          accountDAO.create(account)
        }
      } yield ()
    }

    for {
      _ <- writeF
      nodeTables <- NodeDbManagement.listTables(bhDAO.database)
      walletTables <- WalletDbManagement.listTables(accountDAO.database)
    } yield {
      def hasTable(tables: Seq[SQLiteTableInfo], dao: CRUD[_, _]): Boolean =
        tables.exists(_.name == dao.table.baseTableRow.tableName)

      assert(hasTable(walletTables, accountDAO))
      assert(!hasTable(walletTables, bhDAO))

      assert(hasTable(nodeTables, bhDAO))
      assert(!hasTable(nodeTables, accountDAO))
    }

  }

}
