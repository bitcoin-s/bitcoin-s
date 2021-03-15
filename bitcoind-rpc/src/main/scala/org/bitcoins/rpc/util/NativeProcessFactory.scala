package org.bitcoins.rpc.util

import org.bitcoins.core.util.BitcoinSLogger

import scala.concurrent.{ExecutionContext, Future}
import scala.sys.process.{Process, ProcessBuilder}

/** A trait that helps start bitcoind/eclair when it is started via bitcoin-s */
trait NativeProcessFactory extends BitcoinSLogger {
  implicit protected def executionContext: ExecutionContext

  private[this] var processOpt: Option[Process] = None

  private lazy val process: ProcessBuilder = scala.sys.process.Process(cmd)

  /** The command to start the daemon on the underlying OS */
  def cmd: String

  def isAlive(): Boolean = {
    processOpt match {
      case Some(p) =>
        p.isAlive()
      case None =>
        false
    }
  }

  /** Starts the binary by spinning up a new process */
  def startBinary(): Future[Unit] = Future {
    processOpt match {
      case Some(_) =>
        //don't do anything as it is already started
        logger.debug(s"Binary was already started!")
        ()
      case None =>
        val started = process.run()
        processOpt = Some(started)
        ()
    }
  }

  /** Stops the binary by destroying the underlying operating system process
    *
    * If the client is a remote client (not started on the host operating system)
    * this method is a no-op
    */
  def stopBinary(): Future[Unit] = Future {
    processOpt match {
      case Some(process) =>
        if (process.isAlive()) {
          val _ = process.destroy()
        }
        processOpt = None
      case None =>
        logger.info(s"No process found, binary wasn't started!")
        //no process running, nothing to do
        ()
    }
  }

}
