package org.bitcoins.testkit

//import org.scalatest.{BeforeAndAfterAll, Suite}

/** Lightweight ScalaTest mixin to provide a PostgreSQL Testcontainers instance
  * for tests. Apply this trait to tests that need a Postgres instance in CI or
  * locally.
  *
  * Usage: class MyDaoTest extends AnyFunSuite with TestContainersPostgres { ...
  * }
  *
  * The Postgres container will be started before the suite and stopped after.
  */
//trait TestContainersPostgres extends BeforeAndAfterAll { this: Suite =>
//
//  // Pick a recent, small Postgres image that works well on GitHub Actions runners
//  // (alpine image is small and generally compatible)
//  protected lazy val pgContainer: PostgreSQLContainer =
//    PostgreSQLContainer(dockerImage = "postgres:15-alpine")
//
//  protected def pgJdbcUrl: String = pgContainer.jdbcUrl
//  protected def pgUsername: String = pgContainer.username
//  protected def pgPassword: String = pgContainer.password
//
//  override protected def beforeAll(): Unit = {
//    // Start the container and wait for it to be ready
//    pgContainer.start()
//    super.beforeAll()
//  }
//
//  override protected def afterAll(): Unit = {
//    try pgContainer.stop()
//    finally super.afterAll()
//  }
//}
