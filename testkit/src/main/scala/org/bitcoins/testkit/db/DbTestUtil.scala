package org.bitcoins.testkit.db

import java.nio.file.{Files, Path}

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db._
import scodec.bits.ByteVector
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

object DbTestUtil {

  def createTestDbManagement(testAppConfig: TestAppConfig)(implicit
      system: ActorSystem): TestDbManagement = {
    new TestDbManagement with JdbcProfileComponent[TestAppConfig] {
      override val ec: ExecutionContext = system.dispatcher

      override def appConfig: TestAppConfig = testAppConfig
    }
  }
}

trait TestDbManagement extends DbManagement {
  _: JdbcProfileComponent[TestAppConfig] =>

  import profile.api._

  def ec: ExecutionContext

  private lazy val testTable: TableQuery[Table[_]] =
    TestDAO()(ec, appConfig).table

  override lazy val allTables: List[TableQuery[Table[_]]] =
    List(testTable)

}

case class TestAppConfig(
    private val directory: Path,
    override val useLogbackConf: Boolean,
    private val conf: Config*)(implicit override val ec: ExecutionContext)
    extends AppConfig
    with TestDbManagement
    with JdbcProfileComponent[TestAppConfig] {
  override protected[bitcoins] def configOverrides: List[Config] = conf.toList

  override protected[bitcoins] def moduleName: String = "test"

  override protected[bitcoins] type ConfigType = TestAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): TestAppConfig =
    TestAppConfig(directory, useLogbackConf, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  override def appConfig: TestAppConfig = this

  override def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
    logger.debug(s"Initializing test setup")

    if (Files.notExists(datadir)) {
      Files.createDirectories(datadir)
    }

    createTable(TestDAO()(ec, this).table)
  }

  /** Starts the associated application */
  override def start(): Future[Unit] = FutureUtil.unit
}

case class TestDb(pk: String, data: ByteVector)

case class TestDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: TestAppConfig)
    extends CRUD[TestDb, String]
    with SlickUtil[TestDb, String] {

  import profile.api._

  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)

  import mappers._

  override val table: TableQuery[TestTable] = TableQuery[TestTable]

  override def createAll(ts: Vector[TestDb]): Future[Vector[TestDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ts: Vector[String]): Query[TestTable, TestDb, Seq] = {
    table.filter(_.pk.inSet(ts))
  }

  override def findByPrimaryKey(t: String): Query[TestTable, TestDb, Seq] = {
    table.filter(_.pk === t)
  }

  override def findAll(ts: Vector[TestDb]): Query[TestTable, TestDb, Seq] =
    findByPrimaryKeys(ts.map(_.pk))

  class TestTable(tag: Tag) extends Table[TestDb](tag, "test_table") {

    def pk: Rep[String] = column[String]("pk", O.PrimaryKey)

    def data: Rep[ByteVector] = column[ByteVector]("data")

    def * : ProvenShape[TestDb] =
      (pk, data) <> (TestDb.tupled, TestDb.unapply)
  }
}
