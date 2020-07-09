package org.bitcoins.testkit.db

import org.bitcoins.db.AppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig.ProjectType
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.scalatest._
import org.scalatest.flatspec.FixtureAsyncFlatSpec
import scodec.bits._

import scala.concurrent.Future

sealed trait TestDAOFixture
    extends FixtureAsyncFlatSpec
    with BeforeAndAfterAll
    with BitcoinSFixture
    with EmbeddedPg {

  final override type FixtureParam = TestDAO

  implicit private val testConfig: TestAppConfig = TestAppConfig(
    BitcoinSTestAppConfig.tmpDir(),
    useLogbackConf = true,
    BitcoinSTestAppConfig.configWithEmbeddedDb(Some(ProjectType.Test), pgUrl))

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(testConfig)
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    testConfig.stop()
    ()
  }

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(
      build = () => testConfig.initialize().map(_ => TestDAO()),
      destroy = () => dropAll()
    )(test)
  }

  def dropAll(): Future[Unit] =
    for {
      _ <- testConfig.dropTable("flyway_schema_history")
      _ <- testConfig.dropAll()
    } yield ()

  val testDb: TestDb = TestDb("abc", hex"0054")

  val testDbs: Vector[TestDb] = Vector(TestDb("abc", hex"0050"),
                                       TestDb("abc1", hex"0051"),
                                       TestDb("abc2", hex"0052"),
                                       TestDb("abc3", hex"0053"))

  val updatedDb: TestDb = testDb.copy(data = hex"0000")

  val updatedDbs: Vector[TestDb] = testDbs.map(_.copy(data = hex"0000"))

  def testCreate(testDAO: TestDAO): Future[Boolean] = {
    for {
      _ <- testDAO.create(testDb)
      read <- testDAO.read(testDb.pk)
    } yield read.contains(testDb)
  }

  def testCreateAll(testDAO: TestDAO): Future[Boolean] = {
    for {
      _ <- testDAO.createAll(testDbs)
      read0 <- testDAO.read(id = "abc")
      read1 <- testDAO.read(id = "abc1")
      read2 <- testDAO.read(id = "abc2")
      read3 <- testDAO.read(id = "abc3")
    } yield {
      val read = Vector(read0, read1, read2, read3).flatten
      read == testDbs
    }
  }

  def testDelete(testDAO: TestDAO): Future[Boolean] = {
    for {
      create <- testCreate(testDAO)
      _ = assert(create)

      _ <- testDAO.delete(testDb)
      read2 <- testDAO.read(testDb.pk)
    } yield read2.isEmpty
  }

  def testDeleteAll(testDAO: TestDAO): Future[Boolean] = {
    for {
      created <- testCreateAll(testDAO)
      _ = assert(created)

      _ = testDAO.deleteAll()
      secondRead0 <- testDAO.read(id = "abc")
      secondRead1 <- testDAO.read(id = "abc1")
      secondRead2 <- testDAO.read(id = "abc2")
      secondRead3 <- testDAO.read(id = "abc3")
    } yield {
      val read2 =
        Vector(secondRead0, secondRead1, secondRead2, secondRead3).flatten
      read2 == Vector.empty
    }
  }

  def testUpsert(testDAO: TestDAO): Future[Boolean] = {
    for {
      _ <- testDAO.upsert(testDb)
      read <- testDAO.read(testDb.pk)
      _ = assert(read.contains(testDb))
      _ <- testDAO.upsert(updatedDb)
      read2 <- testDAO.read(testDb.pk)
    } yield read2.contains(updatedDb)
  }

  def testUpsertAll(testDAO: TestDAO): Future[Boolean] = {
    for {
      _ <- testDAO.upsertAll(testDbs)
      read0 <- testDAO.read(id = "abc")
      read1 <- testDAO.read(id = "abc1")
      read2 <- testDAO.read(id = "abc2")
      read3 <- testDAO.read(id = "abc3")
      read = Vector(read0, read1, read2, read3).flatten
      _ = assert(read == testDbs)

      _ <- testDAO.upsertAll(updatedDbs)
      secondRead0 <- testDAO.read(id = "abc")
      secondRead1 <- testDAO.read(id = "abc1")
      secondRead2 <- testDAO.read(id = "abc2")
      secondRead3 <- testDAO.read(id = "abc3")
    } yield {
      val read2 =
        Vector(secondRead0, secondRead1, secondRead2, secondRead3).flatten
      read2 == updatedDbs
    }
  }

  def testUpdate(testDAO: TestDAO): Future[Boolean] = {
    for {
      created <- testCreate(testDAO)
      _ = assert(created)

      _ <- testDAO.update(updatedDb)
      read2 <- testDAO.read(updatedDb.pk)
    } yield read2.contains(updatedDb)
  }

  def testUpdateAll(testDAO: TestDAO): Future[Boolean] = {
    for {
      created <- testCreateAll(testDAO)
      _ = assert(created)

      _ <- testDAO.updateAll(updatedDbs)
      secondRead0 <- testDAO.read(id = "abc")
      secondRead1 <- testDAO.read(id = "abc1")
      secondRead2 <- testDAO.read(id = "abc2")
      secondRead3 <- testDAO.read(id = "abc3")
    } yield {
      val read2 =
        Vector(secondRead0, secondRead1, secondRead2, secondRead3).flatten
      read2 == updatedDbs
    }
  }
}

trait TestSQLiteDAOFixture extends TestDAOFixture {
  override lazy val pgEnabled: Boolean = false
}

trait TestPostgresDAOFixture extends TestDAOFixture {
  override lazy val pgEnabled: Boolean = true
}
