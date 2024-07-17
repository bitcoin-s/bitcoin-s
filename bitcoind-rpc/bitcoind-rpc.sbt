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
    List("29.0", "28.2", "27.2")

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
              "27.2" -> "acc223af46c178064c132b235392476f66d486453ddbd6bca6f1f8411547da78",
              "28.2" -> "98add5f220c01b387343b70edeb6273403fe081e22cd85fda132704cdcaa98aa",
              "29.0" -> "a681e4f6ce524c338a105f214613605bac6c33d58c31dc5135bbc02bc458bb6c"
            )
          else if (Properties.isMac)
            Map(
              "27.2" -> (if (System.getProperty("os.arch") == "aarch64")
                "8f2247f4786f3559d37189b58452c91623efc5fa6886c975fa9386f9ff3f1001"
              else
                "e1efd8c4605b2aabc876da93b6eee2bedd868ce7d1f02b0220c1001f903b3e2c"),
            "28.2" -> (if (System.getProperty("os.arch") == "aarch64")
              "c0270ed50effc174f7ff3332dba5183a8693999dac2ba78b37d8c8797b3ea2b2"
            else
              "e1efd8c4605b2aabc876da93b6eee2bedd868ce7d1f02b0220c1001f903b3e2c"),
              "29.0" -> "34431c582a0399dd42e1276d87d25306cbdde0217f6744bd55a2945986645dda"
            )
          else if (Properties.isWin)
            Map(
              "27.2" -> "82e18f768aa5962b3c002d7f5d6ec9338896804f48406af4b5054c927575dbdf",
              "28.2" -> "da0869639c323bbf6f264f1829083b9514e10179b90c34b09d8cbcab8a1897e3",
              "29.0" -> "4c1780532031129fcacfc0e393c8430b3cea414c9f8c5e0c0c87ebe59a5ada1b"
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
          //val extractCommand = s"""tar -xzf \"$archiveLocation\" --directory \"$binaryDir\""""
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

  //timeout if we cannot download in 5 minutes
  Await.result(downloads, 2.minutes)
}
