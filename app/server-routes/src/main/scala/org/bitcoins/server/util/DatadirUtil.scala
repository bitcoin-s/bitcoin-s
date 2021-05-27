package org.bitcoins.server.util

import com.typesafe.config.Config
import org.bitcoins.core.config.{
  BitcoinNetwork,
  MainNet,
  RegTest,
  SigNet,
  TestNet3
}

import java.nio.file.Path

object DatadirUtil {

  /** Sets the final datadir for our applicatoin.
    * We allow useres to pass in a --datadir command line
    * flag that needs to be used instead of the [[datadir]]
    * specified in bitcoin-s.conf
    */
  def getFinalDatadir(
      datadir: Path,
      baseConfig: Config,
      customFinalDirOpt: Option[String] = None): Path = {
    val usedDir: Path = customFinalDirOpt match {
      case Some(dir) => datadir.resolve(dir)
      case None =>
        val networkStr: String =
          baseConfig.getString("bitcoin-s.network")

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

        val lastDirname = network match {
          case MainNet  => "mainnet"
          case TestNet3 => "testnet3"
          case RegTest  => "regtest"
          case SigNet   => "signet"
        }
        datadir.resolve(lastDirname)
    }

    usedDir
  }
}
