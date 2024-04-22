import java.nio.file.Files
import java.security.MessageDigest
import scala.collection.JavaConverters._
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, Promise}
import scala.util.Properties

name := "bitcoin-s-bitcoind-rpc"

libraryDependencies ++= Deps.bitcoindRpc

CommonSettings.prodSettings

TaskKeys.downloadBitcoind := {
  val logger = streams.value.log
  import scala.sys.process._

  val binaryDir = CommonSettings.binariesPath.resolve("bitcoind")

  if (Files.notExists(binaryDir)) {
    logger.info(s"Creating directory for bitcoind binaries: $binaryDir")
    Files.createDirectories(binaryDir)
  }

  val versions =
    List("25.2","24.2")

  logger.debug(
    s"(Maybe) downloading Bitcoin Core binaries for versions: ${versions.mkString(",")}")

  def getPlatformAndSuffix(version: String): (String, String) = {
    if (Properties.isLinux) ("x86_64-linux-gnu", "tar.gz")
    else if (Properties.isMac) {
      if (versions.contains(version)) {
        if (System.getProperty("os.arch") == "aarch64")
          ("arm64-apple-darwin", "tar.gz")
        else ("x86_64-apple-darwin", "tar.gz")
      } else ("osx64", "tar.gz")
    } else if (Properties.isWin) ("win64", "zip")
    else sys.error(s"Unsupported OS: ${Properties.osName}")
  }

  implicit val ec = scala.concurrent.ExecutionContext.global
  val downloads = versions.map { version =>
    val (platform, suffix) = getPlatformAndSuffix(version)
    val archiveLocation = binaryDir resolve s"$version.$suffix"
    val location =
      if (version.init.endsWith("rc")) { // if it is a release candidate
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
      // copy of FutureUtil.makeAsync
      def makeAsync(func: () => Unit): Future[Unit] = {
        val resultP = Promise[Unit]()

        ec.execute { () =>
          val result: Unit = func()
          resultP.success(result)
        }

        resultP.future
      }

      makeAsync { () =>
        logger.info(
          s"Downloading bitcoind version $version from location: $location")
        logger.info(s"Placing the file in $archiveLocation")
        val downloadCommand = url(location) #> archiveLocation.toFile
        downloadCommand.!!

        val bytes = Files.readAllBytes(archiveLocation)
        val hash = MessageDigest
          .getInstance("SHA-256")
          .digest(bytes)
          .map("%02x" format _)
          .mkString

        val expectedHash =
          if (Properties.isLinux)
            Map(
              "25.2" -> "8d8c387e597e0edfc256f0bbace1dac3ad1ebf4a3c06da3e2975fda333817dea",
              "24.2" -> "7540d6e34c311e355af2fd76e5eee853b76c291978d6b5ebb555c7877e9de38d",
            )
          else if (Properties.isMac)
            Map(
              "25.2" -> (if (System.getProperty("os.arch") == "aarch64")
                "f55b394eebaa11d4b717d68aad9f75b824aaf3a7841dac7c26b1ef3d6d2915f5"
              else
                "e06ba379f6039ca99bc32d3e7974d420a31363498936f88aac7bab6f239de0f5"),
              "24.2" -> (if (System.getProperty("os.arch") == "aarch64")
                "ae6f5f0cb4079005c32695711ef78b26a26c4c547ceb593b3626059626530a5d"
              else
                "b1b21455c339b2daf0998bfad17d0741d967c3c81db040bb5f73234168526d29")
            )
          else if (Properties.isWin)
            Map(
              "25.2" -> "c2ac84f55ee879caefd4414868d318a741c52a7286da190bf7233d86a2ffca69",
              "24.2" -> "544436bc9d5ce017e679bbccfe8a4928fbc840b414ee0240db8c3523ba54340a",
            )
          else sys.error(s"Unsupported OS: ${Properties.osName}")

        val success = hash.equalsIgnoreCase(expectedHash(version))
        if (success) {
          logger.info(s"Download complete and verified, unzipping result")

          val extractCommand =
            s"tar -xzf $archiveLocation --directory $binaryDir"
          logger.info(s"Extracting archive with command: $extractCommand")
          extractCommand.!!
        } else {
          logger.error(
            s"Downloaded invalid version of bitcoind v$version, got $hash, expected ${expectedHash(version)}")
        }

        logger.info(s"Deleting archive")
        Files.delete(archiveLocation)

        if (!success)
          throw new RuntimeException(s"Failed to download bitcoind v$version")
      }

    }
  }

  //timeout if we cannot download in 5 minutes
  Await.result(Future.sequence(downloads), 5.minutes)
}
