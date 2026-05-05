package org.bitcoins.db

import com.typesafe.config.Config
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.db.DatabaseDriver.*
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.db.models.MasterXPubDAO
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig.ProjectType
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.testkit.{BitcoinSTestAppConfig, PostgresTestDatabase}
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.Future

class DbManagementTest extends BitcoinSAsyncTest with PostgresTestDatabase {

  def dbConfig(project: ProjectType): Config = {
    BitcoinSTestAppConfig.configWithEmbeddedDb(Some(project), postgresOpt)
  }

  it must "run migrations for chain db" in {
    val chainAppConfig = ChainAppConfig(
      BitcoinSTestAppConfig.tmpDir(),
      Vector(dbConfig(ProjectType.Chain))
    )
    for {
      _ <- chainAppConfig.start()
      _ = chainAppConfig.driver match {
        case SQLite =>
          val expected = 7
          val flywayInfo = chainAppConfig.info()
          assert(flywayInfo.applied().length == expected)
          assert(flywayInfo.pending().length == 0)
        case PostgreSQL =>
          val expected = 6
          val flywayInfo = chainAppConfig.info()
          // +1 for << Flyway Schema Creation >>
          assert(flywayInfo.applied().length == expected + 1)
          assert(flywayInfo.pending().length == 0)
      }
      _ <- chainAppConfig.stop()
    } yield succeed
  }

  it must "run migrations for dlc db" in {
    val dlcAppConfig = DLCAppConfig(
      BitcoinSTestAppConfig.tmpDir(),
      Vector(dbConfig(ProjectType.DLC))
    )
    for {
      _ <- dlcAppConfig.start()
      _ = dlcAppConfig.driver match {
        case SQLite =>
          val expected = 8
          val flywayInfo = dlcAppConfig.info()
          assert(flywayInfo.applied().length == expected)
          assert(flywayInfo.pending().length == 0)
        case PostgreSQL =>
          val expected = 11
          val flywayInfo = dlcAppConfig.info()
          // +1 for << Flyway Schema Creation >>
          assert(flywayInfo.applied().length == expected + 1)
          assert(flywayInfo.pending().length == 0)
      }
      _ <- dlcAppConfig.stop()
    } yield succeed
  }

  it must "run migrations for wallet db" in {
    val walletAppConfig = WalletAppConfig(
      BitcoinSTestAppConfig.tmpDir(),
      Vector(dbConfig(ProjectType.Wallet))
    )
    for {
      _ <- walletAppConfig.start()
      _ = walletAppConfig.driver match {
        case SQLite =>
          val expected = 18
          val flywayInfo = walletAppConfig.info()
          assert(flywayInfo.applied().length == expected)
          assert(flywayInfo.pending().length == 0)
        case PostgreSQL =>
          val expected = 16
          val flywayInfo = walletAppConfig.info()
          // +1 for << Flyway Schema Creation >>
          assert(flywayInfo.applied().length == expected + 1)
          assert(flywayInfo.pending().length == 0)
      }
      _ = walletAppConfig.clean()
      _ <- walletAppConfig.stop()
    } yield succeed
  }

  it must "clean the wallet db and drop the master_xpub table" in {
    val walletAppConfig = WalletAppConfig(
      BitcoinSTestAppConfig.tmpDir(),
      Vector(dbConfig(ProjectType.Wallet))
    )

    val resultF = for {
      _ <- walletAppConfig.start()
      masterXPubDAO =
        MasterXPubDAO()(using walletAppConfig.ec, appConfig = walletAppConfig)
      beforeExists <- tableExists(walletAppConfig,
                                  masterXPubDAO.table.baseTableRow.tableName)
      _ = assert(beforeExists)
      _ = walletAppConfig.clean()
      afterExists <- tableExists(walletAppConfig,
                                 masterXPubDAO.table.baseTableRow.tableName)
      _ = assert(!afterExists)
    } yield succeed

    resultF.transformWith { outcome =>
      walletAppConfig.stop().transform(_ => outcome)
    }
  }

  it must "run migrations for node db" in {
    val nodeAppConfig = NodeAppConfig(
      BitcoinSTestAppConfig.tmpDir(),
      Vector(dbConfig(ProjectType.Node))
    )
    for {
      _ <- nodeAppConfig.start()
      _ = nodeAppConfig.driver match {
        case SQLite =>
          val expected = 5
          val flywayInfo = nodeAppConfig.info()
          assert(flywayInfo.applied().length == expected)
          assert(flywayInfo.pending().length == 0)
        case PostgreSQL =>
          val expected = 5
          val flywayInfo = nodeAppConfig.info()
          // +1 for << Flyway Schema Creation >>
          assert(flywayInfo.applied().length == expected + 1)
          assert(flywayInfo.pending().length == 0)
      }
      _ <- nodeAppConfig.stop()
    } yield succeed
  }

  it must "run migrations for oracle db" in {
    val oracleAppConfig =
      DLCOracleAppConfig(
        BitcoinSTestAppConfig.tmpDir(),
        Vector(dbConfig(ProjectType.Oracle))
      )
    for {
      _ <- oracleAppConfig.start()
      _ = oracleAppConfig.driver match {
        case SQLite =>
          val expected = 6
          val flywayInfo = oracleAppConfig.info()
          assert(flywayInfo.applied().length == expected)
          assert(flywayInfo.pending().length == 0)
        case PostgreSQL =>
          val expected = 7
          val flywayInfo = oracleAppConfig.info()
          // +1 for << Flyway Schema Creation >>
          assert(flywayInfo.applied().length == expected)
          assert(flywayInfo.pending().length == 0)
      }
      _ <- oracleAppConfig.stop()
    } yield succeed
  }

  import slick.jdbc.meta.MTable

  private def tableExists(
      walletAppConfig: WalletAppConfig,
      tableName: String): Future[Boolean] = {
    val action = MTable.getTables(None,
                                  walletAppConfig.schemaName,
                                  Some(tableName),
                                  Some(Seq("TABLE")))

    walletAppConfig.slickDbConfig.db.run(action).map(_.nonEmpty)
  }
}
