import java.nio.file._

import scala.util.Properties

name := "bitcoin-s-eclair-rpc"

libraryDependencies ++= Deps.eclairRpc

dependsOn(Projects.bitcoindRpc)

CommonSettings.prodSettings

TaskKeys.downloadEclair := {
  val logger = streams.value.log
  import scala.sys.process._

  val binaryDir = CommonSettings.binariesPath.resolve("eclair")

  if (Files.notExists(binaryDir)) {
    logger.info(s"Creating directory for Eclair binaires: $binaryDir")
    Files.createDirectories(binaryDir)
  }

  val version = "0.3.3"
  val commit = "12ac145"

  logger.debug(s"(Maybe) downloading Eclair binaries for version: $version")

  val versionDir = binaryDir resolve version
  val location =
    s"https://github.com/ACINQ/eclair/releases/download/v$version/eclair-node-$version-$commit.jar"

  if (Files.exists(versionDir)) {
    logger.debug(
      s"Directory $versionDir already exists, skipping download of Eclair $version")
  } else {
    logger.info(s"Creating directory $version")
    Files.createDirectories(versionDir)

    val destination = versionDir resolve s"eclair-node-$version-$commit.jar"
    logger.info(
      s"Downloading Eclair $version from location: $location, to destination: $destination")
    (url(location) #> destination.toFile).!!

    logger.info(s"Download complete")
  }
}
