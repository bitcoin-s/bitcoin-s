import java.nio.file._
import java.security.MessageDigest
import scala.util.Properties

name := "bitcoin-s-lnd-rpc"

libraryDependencies ++= Deps.lndRpc

CommonSettings.prodSettings

enablePlugins(AkkaGrpcPlugin)

// Disable deprecation and unused imports warning otherwise generated files will cause errors
Compile / scalacOptions ++= Seq(
  "-Wconf:cat=deprecation:site=lnrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=signrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=walletrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=routerrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=invoicesrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=peersrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=chainrpc\\..*:silent",
  "-Wconf:cat=unused-imports:site=lnrpc:silent",
  "-Wconf:cat=unused-imports:site=signrpc:silent",
  "-Wconf:cat=unused-imports:site=walletrpc:silent",
  "-Wconf:cat=unused-imports:site=routerrpc:silent",
  "-Wconf:cat=unused-imports:site=invoicesrpc:silent",
  "-Wconf:cat=unused-imports:site=peersrpc:silent",
  "-Wconf:cat=unused-imports:site=chainrpc:silent"
)

TaskKeys.downloadLnd := {
  val logger = streams.value.log
  import scala.sys.process._

  val binaryDir = CommonSettings.binariesPath.resolve("lnd")

  if (Files.notExists(binaryDir)) {
    logger.info(s"Creating directory for lnd binaries: $binaryDir")
    Files.createDirectories(binaryDir)
  }

  val version = "0.16.4-beta"

  val (platform, suffix) =
    if (Properties.isLinux) ("linux-amd64", "tar.gz")
    else if (Properties.isMac && System.getProperty("os.arch") == "aarch64")
      ("darwin-arm64", "tar.gz")
    else if (Properties.isMac) ("darwin-amd64", "tar.gz")
    else if (Properties.isWin) ("windows-amd64", "zip")
    else sys.error(s"Unsupported OS: ${Properties.osName}")

  logger.debug(s"(Maybe) downloading lnd binaries for version: $version")

  val versionDir = binaryDir resolve s"lnd-$platform-v$version"
  val location =
    s"https://github.com/lightningnetwork/lnd/releases/download/v$version/lnd-$platform-v$version.$suffix"

  if (Files.exists(versionDir)) {
    logger.debug(
      s"Directory $versionDir already exists, skipping download of lnd $version")
  } else {
    val archiveLocation = binaryDir resolve s"$version.$suffix"
    logger.info(s"Downloading lnd version $version from location: $location")
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
        "4d1bcd7a06aa09786a80f63f0793231d1aeb6b08e6678930d1c0d8110ca554da"
      else if (Properties.isMac && System.getProperty("os.arch") == "aarch64")
        "1cc2a795221b938bcb42adac8e493581d3a0941f4a47acf3b1d6e620e230af70"
      else if (Properties.isMac)
        "10f0000e17b3f035d07ca1f03a7f0c0e984a814f77ff42fb8bacce2eacdfd648"
      else if (Properties.isWin)
        "c931c669169e3201ad1aa93e7efc87e053de3f97806bcb1714b88193d7e9d076"
      else sys.error(s"Unsupported OS: ${Properties.osName}")

    val success = hash.equalsIgnoreCase(expectedHash)
    if (success) {
      logger.info(s"Download complete and verified, unzipping result")

      val extractCommand = s"tar -xzf $archiveLocation --directory $binaryDir"
      logger.info(s"Extracting archive with command: $extractCommand")
      extractCommand.!!
    } else {
      logger.error(
        s"Downloaded invalid version of lnd, got $hash, expected $expectedHash")
    }

    logger.info(s"Deleting archive")
    Files.delete(archiveLocation)

    if (!success) {
      throw new RuntimeException(s"Failed to download lnd v$version")
    }
  }
}
