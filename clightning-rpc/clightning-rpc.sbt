import java.nio.file._
import java.security.MessageDigest
import scala.util.Properties

name := "bitcoin-s-clightning-rpc"

libraryDependencies ++= Deps.clightningRpc

CommonSettings.prodSettings

TaskKeys.downloadCLightning := {
  val logger = streams.value.log
  import scala.sys.process._

  val binaryDir = CommonSettings.binariesPath.resolve("clightning")

  if (Files.notExists(binaryDir)) {
    logger.info(s"Creating directory for clightning binaries: $binaryDir")
    Files.createDirectories(binaryDir)
  }

  val version = "0.10.1"

  val (platform, suffix) =
    if (Properties.isLinux) ("Ubuntu-20.04", "tar.xz")
//    else if (Properties.isMac) ("darwin-amd64", "tar.gz") // todo c-lightning adding in a future release
    else sys.error(s"Unsupported OS: ${Properties.osName}")

  logger.debug(s"(Maybe) downloading clightning binaries for version: $version")

  val versionDir = binaryDir resolve version
  val location =
    s"https://github.com/ElementsProject/lightning/releases/download/v$version/clightning-v$version-$platform.tar.xz"

  if (Files.exists(versionDir)) {
    logger.debug(
      s"Directory $versionDir already exists, skipping download of clightning $version")
  } else {
    Files.createDirectories(versionDir)
    val archiveLocation = binaryDir resolve s"$version.$suffix"
    logger.info(
      s"Downloading clightning version $version from location: $location")
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
        "c99e1879e36f108e4c7d9bef597a4179e6c6b5b92b1142f2bc9e888cf0cf6ea6"
      else sys.error(s"Unsupported OS: ${Properties.osName}")

    if (hash.equalsIgnoreCase(expectedHash)) {
      logger.info(s"Download complete and verified, unzipping result")

      val extractCommand = s"tar -xf $archiveLocation --directory $versionDir"
      logger.info(s"Extracting archive with command: $extractCommand")
      extractCommand.!!
    } else {
      logger.error(
        s"Downloaded invalid version of c-lightning, got $hash, expected $expectedHash")
    }

    logger.info(s"Deleting archive")
    Files.delete(archiveLocation)
  }
}
