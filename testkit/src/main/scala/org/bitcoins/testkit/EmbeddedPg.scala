package org.bitcoins.testkit

import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, Suite}
import org.testcontainers.containers.PostgreSQLContainer

import java.sql.DriverManager

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
    isEnv || isConfig
  }

  // Lazily create a Testcontainers PostgreSQL container when enabled.
  // We configure a small image and reasonable defaults similar to the otj usage.
  private lazy val createdContainer: Option[PostgreSQLContainer[?]] = {
    if (!pgEnabled) None
    else {
      // val pgStartupWait = sys.env.getOrElse("PG_STARTUP_WAIT", "60").toInt
      val image = sys.env.getOrElse("PG_IMAGE", "postgres:15-alpine")
      val container =
        new PostgreSQLContainer(image)
      // Start container and wait for readiness
      container.start()
      Some(container)
    }
  }

  def postgresOpt: Option[PostgreSQLContainer[?]] = createdContainer

  def pgUrl(): Option[String] = postgresOpt
    .map(_.getJdbcUrl)

  def pgUsername(): Option[String] = postgresOpt.map(_.getUsername)
  def pgPassword(): Option[String] = postgresOpt.map(_.getPassword)

  override def afterAll(): Unit = {
    try {
      postgresOpt.foreach(_.stop())
    } finally super.afterAll()
  }

  def executePgSql(sql: String): Unit =
    postgresOpt.foreach { container =>
      val url = container.getJdbcUrl
      val user = container.getUsername
      val pass = container.getPassword
      var conn: java.sql.Connection = null
      try {
        conn = DriverManager.getConnection(url, user, pass)
        val st = conn.createStatement()
        try st.execute(sql)
        finally st.close()
      } catch {
        case ex: Throwable =>
          System.err.println(sql)
          ex.printStackTrace()
      } finally if (conn != null) conn.close()
    }

}
