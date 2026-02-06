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
    List("30.2", "29.2", "28.2")

  logger.debug(
    s"(Maybe) downloading Bitcoin Core binaries for versions: ${versions.mkString(",")}")

  def isAarch64: Boolean = {
    System.getProperty("os.arch") == "aarch64"
  }

  def getPlatformAndSuffix(version: String): (String, String) = {
    if (Properties.isLinux) ("x86_64-linux-gnu", "tar.gz")
    else if (Properties.isMac) {
      if (versions.contains(version)) {
        if (isAarch64)
          ("arm64-apple-darwin", "tar.gz")
        else ("x86_64-apple-darwin", "tar.gz")
      } else ("osx64", "tar.gz")
    } else if (Properties.isWin) ("win64", "zip")
    else sys.error(s"Unsupported OS: ${Properties.osName}")
  }

  implicit val ec = scala.concurrent.ExecutionContext.global
  val downloads = Future.traverse(versions) { version =>
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
              "28.2" -> "98add5f220c01b387343b70edeb6273403fe081e22cd85fda132704cdcaa98aa",
              "29.2" -> "1fd58d0ae94b8a9e21bbaeab7d53395a44976e82bd5492b0a894826c135f9009",
              "30.2" -> "6aa7bb4feb699c4c6262dd23e4004191f6df7f373b5d5978b5bcdd4bb72f75d8"
            )
          else if (Properties.isMac)
            Map(
              "28.2" -> (if (isAarch64)
                           "c0270ed50effc174f7ff3332dba5183a8693999dac2ba78b37d8c8797b3ea2b2"
                         else
                           "e1efd8c4605b2aabc876da93b6eee2bedd868ce7d1f02b0220c1001f903b3e2c"),
              "29.2" -> (if (isAarch64)
                           "bd07450f76d149d094842feab58e6240673120c8a317a1c51d45ba30c34e85ef"
                         else
                           "69ca05fbe838123091cf4d6d2675352f36cf55f49e2e6fb3b52fcf32b5e8dd9f"),
              "30.2" -> (if (isAarch64)
                           "c2ecab62891de22228043815cb6211549a32272be3d5d052ff19847d3420bd10"
                         else
                           "99d5cee9b9c37be506396c30837a4b98e320bfea71c474d6120a7e8eb6075c7b")
            )
          else if (Properties.isWin)
            Map(
              "28.2" -> "da0869639c323bbf6f264f1829083b9514e10179b90c34b09d8cbcab8a1897e3",
              "29.2" -> "83f90a5bab1fc30849862aa1db88906b91e0730b78993c085f9e547a1c3cce79",
              "30.2" -> "0d7e1f16f8823aa26d29b44855ff6dbac11c03d75631a6c1d2ea5fab3a84fdf8"
            )
          else sys.error(s"Unsupported OS: ${Properties.osName}")

        val success = hash.equalsIgnoreCase(expectedHash(version))
        if (success) {
          logger.info(s"Download complete and verified, unzipping result")
          val cmds = Vector(
            "tar",
            "-xzf",
            archiveLocation.toString,
            "--directory",
            binaryDir.toString
          )
          // val extractCommand = s"""tar -xzf \"$archiveLocation\" --directory \"$binaryDir\""""
          logger.info(s"Extracting archive with command: $cmds")
          cmds.!!
        } else {
          Files.deleteIfExists(expectedEndLocation)
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

  // timeout if we cannot download in 5 minutes
  Await.result(downloads, 2.minutes)
}
