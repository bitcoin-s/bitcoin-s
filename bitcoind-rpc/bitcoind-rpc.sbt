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
    List("31.1", "30.3", "29.4")

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
              "29.4" -> "e15bff6f6d21a315c4af25d2e8ae933a22bd51e924e0e90ab0474e1e11516331",
              "30.3" -> "4753cd70416c629a2090503e8545a3c05e94a0f5037f8fb97b4c35c6e1afee0e",
              "31.1" -> "b80d9c3e04da78fb6f0569685673418cf686fadba9042d926d13fb87ff503f9e"
            )
          else if (Properties.isMac)
            Map(
              "29.4" -> (if (isAarch64)
                           "ab9d71a1fe9b32a284b3456fd62e209c7d5d08ddfa2534d048c3f6e610cdb37a"
                         else
                           "b2e13a7f4f430c52ca96a4fca8f041b0a23ea8ea97267357751cdd9de7606cf2"),
              "30.3" -> (if (isAarch64)
                           "c42480fd26dd0b12c984e8063a1879165c94525c475162deb3ea2c3054dd5c2c"
                         else
                           "24897a2e40596df5e4206499935aac0fe045c58b3de3170ce92ebbb2ebc765a3"),
              "31.1" -> (if (isAarch64)
                           "16a097c09fbd7eb78b240ce1dae123663ea2e5e377cfd6a951e71e227e23cf2f"
                         else
                           "bc506958d0f387c1ea770bdc7c7192a505fa645ff62cabcc7761fa7eb89e867e")
            )
          else if (Properties.isWin)
            Map(
              "29.4" -> "31e03b841bf2bbe711cf0179d3466678989fcbd46e5ef9bef957a20fa32e0e42",
              "30.3" -> "5fa956b48c358148ff83c1338648d2b82e2e05e193e362ee2f8e76a59bc87c0f",
              "31.1" -> "c99ef173471c58e6766d9eebd12e6c35349082eeed3939bc99eed58ef57db587"
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
