package org.bitcoins.cli.grpc

import org.apache.pekko.actor.ActorSystem

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.control.NonFatal

object CliGrpc extends App {
  System.setProperty("slf4j.internal.verbosity", "WARN")
  import System.err.{println => printerr}

  implicit val system: ActorSystem = ActorSystem("bitcoin-s-cli-grpc")

  val exitCode =
    try {
      val output = Await.result(ConsoleCliGrpc.exec(args.toVector), 30.seconds)
      println(output)
      0
    } catch {
      case NonFatal(err) =>
        printerr(err.getMessage)
        1
    } finally {
      Await.result(system.terminate(), 10.seconds)
      ()
    }

  sys.exit(exitCode)
}
