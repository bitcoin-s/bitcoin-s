import java.nio.file._
import java.security.MessageDigest
import scala.util.Properties

name := "bitcoin-s-lnd-rpc"

libraryDependencies ++= Deps.lndRpc

CommonSettings.prodSettings

enablePlugins(PekkoGrpcPlugin)

// Disable deprecation and unused imports warning otherwise generated files will cause errors
Compile / scalacOptions ++= Seq(
  "-Wconf:cat=deprecation:site=lnrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=signrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=walletrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=routerrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=invoicesrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=peersrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=chainrpc\\..*:silent",
  "-Wconf:cat=deprecation:site=wtclientrpc\\..*:silent",
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

  val version = "0.20.1-beta"

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
        "e01f755ba18e45a7b20f9fd645a328a250aae241e23b8c1eca06efeb2974570a"
      else if (Properties.isMac && System.getProperty("os.arch") == "aarch64")
        "c6e85d512c218d24913f9abd7b74d700d325e47f5ddc1aee671016025a7841fe"
      else if (Properties.isMac)
        "0e0a68086a862ecf88145cf8adf62845d1c37a78a3fb5233e58490cf40ddc006"
      else if (Properties.isWin)
        "9aa0327ff302b1ce7e3d4079c0f1a5d01d26238339158be098b8b4a2b30d0bef"
      else sys.error(s"Unsupported OS: ${Properties.osName}")

    val success = hash.equalsIgnoreCase(expectedHash)
    if (success) {
      logger.info(s"Download complete and verified, unzipping result")
      val cmds = Vector(
        "tar",
        "-xzf",
        archiveLocation.toString,
        "--directory",
        binaryDir.toString
      )
      logger.info(s"Extracting archive with command: $cmds")
      cmds.!!
    } else {
      Files.deleteIfExists(versionDir)
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
