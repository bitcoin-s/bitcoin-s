import java.nio.file.Files
import scala.collection.JavaConverters._
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Properties

name := "bitcoin-s-bitcoind-rpc"

libraryDependencies ++= Deps.bitcoindRpc

dependsOn(Projects.core)

CommonSettings.prodSettings

TaskKeys.downloadBitcoind := {
  val logger = streams.value.log
  import scala.sys.process._

  val binaryDir = CommonSettings.binariesPath.resolve("bitcoind")

  if (Files.notExists(binaryDir)) {
    logger.info(s"Creating directory for bitcoind binaries: $binaryDir")
    Files.createDirectories(binaryDir)
  }

  val experimentalVersion =
    "0.18.99" // TODO: change this when new version compiled on suredbits server

  val versions =
    List("0.21.0",
         "0.20.1",
         "0.19.0.1",
         "0.18.1",
         "0.17.0.1",
         "0.16.3",
         experimentalVersion)

  logger.debug(
    s"(Maybe) downloading Bitcoin Core binaries for versions: ${versions.mkString(",")}")

  val (platform, suffix) =
    if (Properties.isLinux) ("x86_64-linux-gnu", "tar.gz")
    else if (Properties.isMac) ("osx64", "tar.gz")
    else if (Properties.isWin) ("win64", "zip")
    else sys.error(s"Unsupported OS: ${Properties.osName}")

  implicit val ec = scala.concurrent.ExecutionContext.global
  val downloads = versions.map { version =>
    val versionDir = binaryDir resolve version
    val archiveLocation = binaryDir resolve s"$version.$suffix"
    val location =
      if (version == experimentalVersion)
        s"https://s3-us-west-1.amazonaws.com/suredbits.com/bitcoin-core-$version/bitcoin-$version-$platform.$suffix"
      else if (version.init.endsWith("rc")) { // if it is a release candidate
        val (base, rc) = version.splitAt(version.length - 3)
        s"https://bitcoincore.org/bin/bitcoin-core-$base/test.$rc/bitcoin-$version-$platform.$suffix"
      } else
        s"https://bitcoincore.org/bin/bitcoin-core-$version/bitcoin-$version-$platform.$suffix"

    val expectedEndLocation = binaryDir resolve s"bitcoin-$version"

    if (
      Files
        .list(binaryDir)
        .iterator
        .asScala
        .map(_.toString)
        .exists(expectedEndLocation.toString.startsWith(_))
    ) {
      logger.debug(
        s"Directory $expectedEndLocation already exists, skipping download of version $version")
      Future.unit
    } else {
      Future {
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

  //timeout if we cannot download in 5 minutes
  Await.result(Future.sequence(downloads), 5.minutes)
}
