package org.bitcoins.testkit.db

import java.nio.file.Files

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.hd.{HDAccount, HDCoin, HDCoinType, HDPurposes}
import org.bitcoins.db.{CRUD, SQLiteTableInfo}
import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.testkit.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSAppConfig._
import org.bitcoins.testkit.chain.ChainTestUtil
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.models.{AccountDAO, AccountDb}

import scala.concurrent.ExecutionContext

class AppConfigTest extends BitcoinSUnitTest {

  val system = ActorSystem()
  implicit val ec: ExecutionContext = system.dispatcher

  behavior of "BitcoinSAppConfig"

  it must "propagate values correctly to all sub configs" in {
    val networkOverride =
      ConfigFactory.parseString("bitcoin-s.network = testnet3")

    val config = BitcoinSAppConfig.getTestConfig(networkOverride)
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
    val conf = BitcoinSAppConfig.getTestConfig()
    val chainConf = conf.chainConf
    val walletConf = conf.walletConf
    val nodeConf = conf.nodeConf
    assert(chainConf.dbPath == walletConf.dbPath)
    assert(walletConf.dbPath == nodeConf.dbPath)
  }

  it must "have distinct databases" in {
    val conf = BitcoinSAppConfig.getTestConfig()
    val chainConf = conf.chainConf
    val walletConf = conf.walletConf
    val nodeConf = conf.nodeConf
    assert(chainConf.dbName != walletConf.dbName)
    assert(walletConf.dbName != nodeConf.dbName)
  }

  it must "be able to write to distinct databases" in {
    implicit val config = BitcoinSAppConfig.getTestConfig()
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
          val xpub = CryptoGenerators.extPublicKey.sample.get
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
