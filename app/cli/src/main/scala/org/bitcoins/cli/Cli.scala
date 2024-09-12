package org.bitcoins.cli

import java.net.ConnectException

import scala.util.{Failure, Success}

object Cli extends App {
  // see: https://github.com/qos-ch/slf4j/issues/422
  System.setProperty("slf4j.internal.verbosity", "WARN")
  import System.err.{println => printerr}

  try {
    ConsoleCli.exec(args.toVector) match {
      case Success(output) => println(output)
      case Failure(err) =>
        printerr(err.getMessage)
        sys.exit(1)
    }
  } catch {
    case _: ConnectException =>
      printerr(
        "Connection refused! Check that the server is running and configured correctly."
      )
      sys.exit(1)
  }
}
