import scala.util.Properties
import scala.collection.JavaConverters._
import java.nio.file.Files
import java.nio.file.Paths

name := "bitcoin-s-bitcoind-rpc"

libraryDependencies ++= Deps.bitcoindRpc

dependsOn {
  lazy val core = project in Paths.get("..", "core").toFile
  core
}

lazy val downloadBitcoind = taskKey[Unit] {
  "Download bitcoind binaries, extract to ./binaries/bitcoind"
}

downloadBitcoind := {
  val logger = streams.value.log
  import scala.sys.process._

  val binaryDir = Paths.get("binaries", "bitcoind")

  if (Files.notExists(binaryDir)) {
    logger.info(s"Creating directory for bitcoind binaries: $binaryDir")
    Files.createDirectories(binaryDir)
  }

  val versions = List("0.17.0.1", "0.16.3")

  logger.debug(
    s"(Maybe) downloading Bitcoin Core binaries for versions: ${versions.mkString(",")}")

  val platform =
    if (Properties.isLinux) "x86_64-linux-gnu"
    else if (Properties.isMac) "osx64"
    else sys.error(s"Unsupported OS: ${Properties.osName}")

  versions.foreach { version =>
    val versionDir = binaryDir resolve version
    val archiveLocation = binaryDir resolve s"$version.tar.gz"
    val location =
      s"https://bitcoincore.org/bin/bitcoin-core-$version/bitcoin-$version-$platform.tar.gz"

    val expectedEndLocation = binaryDir resolve s"bitcoin-$version"
    if (Files
          .list(binaryDir)
          .iterator
          .asScala
          .map(_.toString)
          .exists(expectedEndLocation.toString.startsWith(_))) {
      logger.debug(
        s"Directory $expectedEndLocation already exists, skipping download of version $version")
    } else {
      logger.info(
        s"Downloading bitcoind version $version from location: $location")
      logger.info(s"Placing the file in $archiveLocation")
      val downloadCommand = url(location) #> archiveLocation.toFile
      downloadCommand.!!

      logger.info(s"Download complete, unzipping result")

      val extractCommand = s"tar -xzf $archiveLocation --directory $binaryDir"
      logger.info(s"Extracting archive with command: $extractCommand")
      extractCommand.!!

      logger.info(s"Deleting archive")
      Files.delete(archiveLocation)
    }
  }
}
