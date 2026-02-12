package org.bitcoins.testkit

import com.dimafeng.testcontainers.PostgreSQLContainer
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, Suite}
import org.testcontainers.utility.DockerImageName

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
  private lazy val createdContainer: Option[PostgreSQLContainer] = {
    if (!pgEnabled) None
    else {
      val pgStartupWait = sys.env.getOrElse("PG_STARTUP_WAIT", "60").toInt
      val image = sys.env.getOrElse("PG_IMAGE", "postgres:15-alpine")
      val container =
        PostgreSQLContainer(dockerImage = DockerImageName.parse(image))
      // Start container and wait for readiness
      container.start()
      Some(container)
    }
  }

  def pg: Option[PostgreSQLContainer] = createdContainer

  def pgUrl(): Option[String] = pg.map(_.jdbcUrl)

  override def afterAll(): Unit = {
    try {
      pg.foreach(_.stop())
    } finally super.afterAll()
  }

  def executePgSql(sql: String): Unit =
    pg.foreach { container =>
      val url = container.jdbcUrl
      val user = container.username
      val pass = container.password
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
