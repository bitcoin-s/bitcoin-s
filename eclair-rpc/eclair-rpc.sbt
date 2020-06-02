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

  val version = "0.4"
  val commit = "69c538e"

  logger.debug(s"(Maybe) downloading Eclair binaries for version: $version")

  val versionDir = binaryDir resolve version
  val location =
    s"https://github.com/ACINQ/eclair/releases/download/v$version/eclair-node-$version-$commit-bin.zip"

  if (Files.exists(versionDir)) {
    logger.debug(
      s"Directory $versionDir already exists, skipping download of Eclair $version")
  } else {
    logger.info(s"Creating directory $version")
    Files.createDirectories(versionDir)

    val archiveLocation = versionDir resolve s"eclair-node-$version-$commit.zip"
    logger.info(
      s"Downloading Eclair $version from location: $location, to destination: $archiveLocation")
    (url(location) #> archiveLocation.toFile).!!

    val extractCommand = s"unzip $archiveLocation -d $versionDir"
    logger.info(s"Extracting archive with command: $extractCommand")
    extractCommand.!!

    logger.info(s"Deleting archive")
    Files.delete(archiveLocation)

    fixShebang(
      versionDir resolve s"eclair-node-$version-$commit" resolve "bin" resolve "eclair-node.sh")

    logger.info(s"Download complete")
  }

  // remove me when https://github.com/ACINQ/eclair/issues/1421
  // and https://github.com/ACINQ/eclair/issues/1422 are fixed
  def fixShebang(scriptPath: Path): Unit = {
    import java.nio.file.attribute.PosixFilePermissions
    import scala.io.Source
    import scala.collection.JavaConverters._

    val tempPath = scriptPath.getParent resolve scriptPath.getFileName.toString + ".tmp"
    Files.createFile(tempPath,
                     PosixFilePermissions.asFileAttribute(
                       PosixFilePermissions.fromString("rwxr-xr-x")))
    val source = Source
      .fromFile(scriptPath.toUri)

    val lines = (Vector("#!/usr/bin/env bash") ++ source.getLines()).map(
      line =>
        if (line == "declare -r lib_dir=\"$(realpath \"${app_home::-4}/lib\")\" # {app_home::-4} transforms ../bin in ../")
          "declare -r lib_dir=\"$(realpath \"${app_home:0:${#app_home}-4}/lib\")\" # {app_home:0:${#app_home}-4} transforms ../bin in ../"
        else line)

    source.close()

    Files.write(tempPath, lines.asJava, StandardOpenOption.WRITE)

    tempPath.toFile.renameTo(scriptPath.toFile)
  }
}
