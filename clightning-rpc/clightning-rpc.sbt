import java.nio.file._
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

    logger.info(s"Download complete, unzipping result")

    val extractCommand =
      s"tar -xf $archiveLocation --directory $versionDir"
    logger.info(s"Extracting archive with command: $extractCommand")
    extractCommand.!!

    logger.info(s"Deleting archive")
    Files.delete(archiveLocation)
  }
}
