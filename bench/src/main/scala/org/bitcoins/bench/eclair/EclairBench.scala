package org.bitcoins.bench.eclair

import java.io.File
import java.nio.file.{Files, StandardOpenOption}

import akka.actor.ActorSystem
import org.bitcoins.core.protocol.ln.currency._
import org.bitcoins.eclair.rpc.api.PaymentId
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.eclair.rpc.EclairRpcTestUtil

import scala.collection.JavaConverters._
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object EclairBench extends App with EclairRpcTestUtil {

  import PaymentLog._

  implicit val system = ActorSystem()
  import system.dispatcher

  val networkSize = 1
  val paymentCount = 10
  val channelAmount = 10000000000L.msats
  val outputFileName = "test.csv"

  // release
  val EclairVersion = Option.empty[String]
  val EclairCommit = Option.empty[String]

  // psql
  // compiled binary can be found here:
  // https://s3-us-west-1.amazonaws.com/suredbits.com/eclair/eclair-node-0.3.3-SNAPSHOT-949f1ec-psql.jar
  // put it into binaries/eclair/0.3.3-SNAPSHOT directory
//  val EclairVersion = Option("0.3.3-SNAPSHOT")
//  val EclairCommit = Option("949f1ec-psql")

  // don't forget to recreate `eclair` Postgres database before starting a new test
  EclairRpcTestUtil.customConfigMap = Map(
    "eclair.db.driver" -> "psql"
  )

  def sendPayments(network: Network, amount: MilliSatoshis, count: Int)(
      implicit ec: ExecutionContext): Future[Vector[PaymentId]] =
    for {
      testNodeInfo <- network.testEclairNode.getInfo
      paymentIds <- Future.sequence(network.networkEclairNodes.map { node =>
        1.to(count).foldLeft(Future.successful(Vector.empty[PaymentId])) {
          (accF, _) =>
            for {
              acc <- accF
              invoice <- network.testEclairNode
                .createInvoice("test " + System.currentTimeMillis(), amount)
              paymentHash = invoice.lnTags.paymentHash.hash
              _ = logPaymentHash(paymentHash)
              id <- node.sendToNode(testNodeInfo.nodeId,
                                    invoice.amount.get.toMSat,
                                    invoice.lnTags.paymentHash.hash,
                                    None,
                                    None,
                                    None,
                                    None)
            } yield {
              logPaymentId(paymentHash, id)
              acc :+ id
            }
        }
      })
    } yield paymentIds.flatten

  def runTests(network: Network): Future[Vector[PaymentLogEntry]] = {
    println("Setting up the test network")
    for {
      _ <- network.testEclairNode.connectToWebSocket(logEvent)
      _ = println(
        s"Set up ${networkSize} nodes, that will send $paymentCount paments to the test node each")
      _ = println(
        s"Test node data directory: ${network.testEclairNode.instance.authCredentials.datadir
          .getOrElse("")}")
      _ = println("Testing...")
      _ <- sendPayments(network, 1000.msats, paymentCount)
      _ <- TestAsyncUtil.retryUntilSatisfied(
        condition = paymentLog.size() == networkSize * paymentCount,
        duration = 1.second,
        maxTries = 100)
      _ <- TestAsyncUtil
        .retryUntilSatisfied(condition =
                               paymentLog.values().asScala.forall(_.completed),
                             duration = 1.second,
                             maxTries = 100)
      _ = println("Done!")
    } yield {
      paymentLog
        .values()
        .asScala
        .toVector
        .sortBy(_.paymentSentAt)
    }
  }

  val res: Future[Unit] = for {
    network <- Network.start(EclairVersion,
                             EclairCommit,
                             networkSize,
                             channelAmount)
    log <- runTests(network).recoverWith {
      case e: Throwable =>
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
            .map {
              case (x, i) =>
                s"${x.paymentSentAt - first.paymentSentAt},${i + 1},${x.toCSV}"
            }
      val outputFile = new File(outputFileName)
      Files.write(outputFile.toPath,
                  csv.asJava,
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
