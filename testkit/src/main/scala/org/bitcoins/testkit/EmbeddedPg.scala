package org.bitcoins.testkit

import com.opentable.db.postgres.embedded.EmbeddedPostgres
import org.bitcoins.testkit.BitcoinSTestAppConfig.ProjectType
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.util.Try

trait EmbeddedPg extends BeforeAndAfterAll { this: Suite =>

  val pgEnabled: Boolean = sys.env.contains("PG_ENABLED")

  val pg: Option[EmbeddedPostgres] =
    if (pgEnabled) Some(EmbeddedPostgres.start()) else None

  def pgUrl(dbname: String): Option[String] =
    pg.map(_.getJdbcUrl("postgres", dbname))

  def pgUrl(project: ProjectType): Option[String] =
    project match {
      case ProjectType.Wallet => pgUrl("walletdb")
      case ProjectType.Node   => pgUrl("nodedb")
      case ProjectType.Chain  => pgUrl("chaindb")
    }

  override def beforeAll(): Unit = {
    super.beforeAll()
    executePgSql(s"CREATE DATABASE chaindb")
    executePgSql(s"CREATE DATABASE walletdb")
    executePgSql(s"CREATE DATABASE nodedb")
  }

  override def afterAll(): Unit = {
    super.afterAll()
    Try(executePgSql(s"DROP DATABASE nodedb"))
    Try(executePgSql(s"DROP DATABASE walletdb"))
    Try(executePgSql(s"DROP DATABASE chaindb"))
    Try(pg.foreach(_.close()))
    ()
  }

  def executePgSql(sql: String): Unit =
    pg.foreach { pg =>
      val conn = pg.getPostgresDatabase.getConnection
      try {
        val st = conn.createStatement()
        try {
          st.execute(sql)
        } finally st.close()

      } finally conn.close()
    }

}
