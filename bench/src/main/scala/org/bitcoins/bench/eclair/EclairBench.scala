package org.bitcoins.bench.eclair

import java.io.File
import java.nio.file.{Files, StandardOpenOption}

import akka.actor.ActorSystem
import org.bitcoins.commons.jsonmodels.eclair.PaymentId
import org.bitcoins.core.protocol.ln.currency._
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.eclair.rpc.EclairRpcTestUtil

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

/** This test spins up one test node and [[NetworkSize]] sender nodes, which open channels with the test one.
  * Then each sender node sends [[PaymentCount]] payments to the test node one by one. For each payment the
  * test node generates an invoice and the send node pays it using `sendtonode` API call.
  *
  * The test keeps track of times when a payment was initiated, when the payment ID was received,
  * and when the corresponding web socket event was received. It writes all results into [[OutputFileName]]
  * in CSV format.
  */
object EclairBench extends App with EclairRpcTestUtil {

  import PaymentLog._

  implicit val system: ActorSystem = ActorSystem()
  import system.dispatcher

  // put compiled test jar files into binaries/eclair/${version} directory

  // None means current release
  val TestEclairVersion = Option.empty[String]
  val TestEclairCommit = Option.empty[String]
  //  val TestEclairVersion = Option("0.3.3-SNAPSHOT")
  //  val TestEclairCommit = Option("84825ff")
  val SenderEclairVersion = Option.empty[String]
  val SenderEclairCommit = Option.empty[String]

  val NetworkSize = 10
  val PaymentCount = 2000
  val ChannelAmount = 10000000000L.msats
  val PaymentAmount = 10.msats
  val OutputFileName = "test.csv"
  val LogbackXml = None // Some("~/logback.xml")

  // don't forget to recreate `eclair` Postgres database before starting a new test
  EclairRpcTestUtil.customConfigMap = Map(
    "eclair.db.driver" -> "psql",
//    "eclair.db.psql.pool.max-size" -> 12,
    "eclair.db.psql.lock-type" -> "none"
//    "eclair.db.psql.lock-type" -> "optimistic"
//    "eclair.db.psql.lock-type" -> "exclusive"
  )

  object Progress {
    private var count = 0
    private var percentage = 0

    def inc(): Unit =
      synchronized {
        count += 1
        val newPercentage = count * 100 / (NetworkSize * PaymentCount)
        if (newPercentage % 10 == 0 && newPercentage != percentage) {
          percentage = newPercentage
          print(s"$percentage% ")
        }
      }
  }

  def sendPayments(
      network: EclairNetwork,
      amount: MilliSatoshis,
      count: Int): Future[Vector[PaymentId]] =
    for {
      _ <- network.testEclairNode.getInfo
      paymentIds <- Future.sequence(network.networkEclairNodes.map { node =>
        1.to(count).foldLeft(Future.successful(Vector.empty[PaymentId])) {
          (accF, _) =>
            for {
              acc <- accF
              invoice <-
                network.testEclairNode
                  .createInvoice("test " + System.currentTimeMillis(), amount)
              paymentHash = invoice.lnTags.paymentHash.hash
              _ = logPaymentHash(paymentHash)
              p = promises.get(paymentHash)
              id <- node.payInvoice(invoice)
              _ = logPaymentId(paymentHash, id)
              _ <- p.future
            } yield {
              Progress.inc()
              acc :+ id
            }
        }
      })
    } yield paymentIds.flatten

  def runTests(network: EclairNetwork): Future[Vector[PaymentLogEntry]] = {
    println("Setting up the test network")
    for {
      _ <- network.testEclairNode.connectToWebSocket { event =>
        val _ = logEvent(event)
      }
      _ = println(
        s"Set up $NetworkSize nodes, that will send $PaymentCount payments to the test node each")
      _ = println(
        s"Test node data directory: ${network.testEclairNode.instance.authCredentials.datadir
          .getOrElse("")}")
      _ = println("Testing...")
      _ <- sendPayments(network, PaymentAmount, PaymentCount)
      _ <- TestAsyncUtil.retryUntilSatisfied(
        condition = paymentLog.size() == NetworkSize * PaymentCount,
        interval = 1.second,
        maxTries = 100)
      _ <-
        TestAsyncUtil
          .retryUntilSatisfied(
            condition = EclairBenchUtil.paymentLogValues().forall(_.completed),
            interval = 1.second,
            maxTries = 100)
          .recover { case ex: Throwable => ex.printStackTrace() }
      _ = println("\nDone!")
    } yield {
      EclairBenchUtil.paymentLogValues().sortBy(_.paymentSentAt)
    }
  }

  val res: Future[Unit] = for {
    network <- EclairNetwork.start(TestEclairVersion,
                                   TestEclairCommit,
                                   SenderEclairVersion,
                                   SenderEclairCommit,
                                   NetworkSize,
                                   ChannelAmount,
                                   LogbackXml)
    log <- runTests(network).recoverWith { case e: Throwable =>
      e.printStackTrace()
      Future.successful(Vector.empty[PaymentLogEntry])
    }
    _ <- network.shutdown()
  } yield {
    if (log.nonEmpty) {
      val first = log.head
      val csv =
        Vector(
          "time,number_of_payments,payment_hash,payment_id,event,payment_sent_at,payment_id_received_at,event_received_at,received_in,completed_in") ++
          log.zipWithIndex
            .map { case (x, i) =>
              s"${x.paymentSentAt - first.paymentSentAt},${i + 1},${x.toCSV}"
            }
      val outputFile = new File(OutputFileName)
      Files.write(outputFile.toPath,
                  EclairBenchUtil.convertStrings(csv),
                  StandardOpenOption.CREATE,
                  StandardOpenOption.WRITE,
                  StandardOpenOption.TRUNCATE_EXISTING)
      println(s"The test results was written in ${outputFile.getAbsolutePath}")
    }
  }

  res.onComplete { e =>
    e match {
      case Success(_)  => ()
      case Failure(ex) => ex.printStackTrace()
    }
    sys.exit()
  }
}
