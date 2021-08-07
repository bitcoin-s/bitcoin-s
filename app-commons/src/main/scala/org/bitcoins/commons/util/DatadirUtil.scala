package org.bitcoins.commons.util

import com.typesafe.config.Config
import org.bitcoins.core.config._

import java.nio.file.Path
import scala.util.Properties

object DatadirUtil {

  def networkStrToDirName(networkStr: String): String = {
    val network: BitcoinNetwork = networkStr.toLowerCase match {
      case "mainnet"  => MainNet
      case "main"     => MainNet
      case "testnet3" => TestNet3
      case "testnet"  => TestNet3
      case "test"     => TestNet3
      case "regtest"  => RegTest
      case "signet"   => SigNet
      case "sig"      => SigNet
      case _: String =>
        throw new IllegalArgumentException(s"Invalid network $networkStr")
    }

    network match {
      case MainNet  => "mainnet"
      case TestNet3 => "testnet3"
      case RegTest  => "regtest"
      case SigNet   => "signet"
    }
  }

  /** Sets the final datadir for our applicatoin.
    * We allow useres to pass in a --datadir command line
    * flag that needs to be used instead of the [[datadir]]
    * specified in bitcoin-s.conf
    */
  def getFinalDatadir(
      datadir: Path,
      baseConfig: Config,
      customFinalDirOpt: Option[String] = None): Path = {

    // $HOME is not set for windows, need to manually set it
    if (Properties.isWin) {
      System.setProperty("HOME", datadir.getParent.toAbsolutePath.toString)
    }

    val usedDir: Path = customFinalDirOpt match {
      case Some(dir) => datadir.resolve(dir)
      case None =>
        val networkStr: String =
          baseConfig.getString("bitcoin-s.network")

        val lastDirname = networkStrToDirName(networkStr)

        datadir.resolve(lastDirname)
    }

    usedDir
  }
}
