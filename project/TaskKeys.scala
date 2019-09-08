import sbt._

object TaskKeys {
  lazy val downloadBitcoind = taskKey[Unit] {
    "Download bitcoind binaries, extract to ./binaries/bitcoind"
  }

  lazy val downloadEclair = taskKey[Unit] {
    "Download Eclair binaries, extract ./binaries/eclair"
  }
}
