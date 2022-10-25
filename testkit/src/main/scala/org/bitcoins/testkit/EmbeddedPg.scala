package org.bitcoins.testkit

import com.opentable.db.postgres.embedded.EmbeddedPostgres
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, Suite}

trait EmbeddedPg extends BeforeAndAfterAll { this: Suite =>

  lazy val pgEnabled: Boolean = {
    val config = ConfigFactory.load()
    val isEnv = sys.env
      .get("PG_ENABLED")
      .exists(s => s.equalsIgnoreCase("true") || s == "1")

    val isConfig = {
      if (config.hasPath("bitcoin-s.testkit.pg.enabled")) {
        config.getBoolean("bitcoin-s.testkit.pg.enabled")
      } else {
        false
      }
    }
    val isPgEnabled = isEnv || isConfig
    isPgEnabled
  }

  lazy val pg: Option[EmbeddedPostgres] = {

    if (pgEnabled) {
      val pgStartupWait = sys.env.getOrElse("PG_STARTUP_WAIT", "60").toInt
      val p = EmbeddedPostgres
        .builder()
        .setPGStartupWait(java.time.Duration.ofSeconds(pgStartupWait))
        .setServerConfig("max_connections", "25")
        .setServerConfig("shared_buffers", "1MB")
        .start()
      Some(p)
    } else {
      None
    }
  }

  def pgUrl(): Option[String] =
    pg.map(_.getJdbcUrl("postgres"))

  override def afterAll(): Unit = {
    super.afterAll()

    val _ = pg.foreach { p =>
      p.close()
    }
    ()
  }

  def executePgSql(sql: String): Unit =
    pg.foreach { pg =>
      try {
        val conn = pg.getPostgresDatabase.getConnection
        try {
          val st = conn.createStatement()
          try {
            st.execute(sql)
          } finally st.close()
        } finally conn.close()
      } catch {
        case ex: Throwable =>
          System.err.println(sql)
          ex.printStackTrace()
      }
    }

}
