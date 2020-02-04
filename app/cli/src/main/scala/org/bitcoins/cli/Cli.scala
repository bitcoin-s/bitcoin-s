package org.bitcoins.cli

import java.net.ConnectException

import scala.util.{Failure, Success}

object Cli extends App {

  import System.err.{println => printerr}

  try {
    ConsoleCli.exec(args.toVector: _*) match {
      case Success(output) => println(output)
      case Failure(err) =>
        printerr(err.getMessage)
        sys.exit(1)
    }
  } catch {
    case _: ConnectException =>
      printerr(
        "Connection refused! Check that the server is running and configured correctly.")
      sys.exit(1)
  }
}
