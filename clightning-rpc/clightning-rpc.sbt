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

  val version = "25.09"

  val (platform, suffix) =
    if (Properties.isLinux) {
      //from: https://stackoverflow.com/a/51614324/967713
      val processBuilder = new java.lang.ProcessBuilder("lsb_release", "-rs")
      val inputStream = new java.io.InputStreamReader(processBuilder.start().getInputStream())
      val version = new java.io.BufferedReader(inputStream).readLine()
      if (version == "20.04") {
        ("Ubuntu-20.04-amd64", "tar.xz")
      } else if (version == "22.04")  {
        ("Ubuntu-22.04-amd64", "tar.xz")
      } else if (version == "24.04") {
        ("Ubuntu-24.04-amd64", "tar.xz")
      } else
        sys.error(s"Unsupported OS version=$version")
    } else {
      sys.error(s"Unsupported OS: ${Properties.osName}")
    }

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
      if (platform == "Ubuntu-20.04-amd64") {
        "d9bdd4f57dd2b617a0321162fb8983af8245314b69b5669fc69296b1f6ac3ceb"
      } else if (platform == "Ubuntu-22.04-amd64") {
        "4930477574b878dee4f0921df8e7d8db00958965de30645194699d2c31c2bb71"
      } else if (platform == "Ubuntu-24.04-amd64") {
        "63bca7fae6706e6fffc10b15e05c13c275eb220a64991dafc123fa29ac95f8af"
      }
      else sys.error(s"Unsupported OS: ${Properties.osName}")

    val success = hash.equalsIgnoreCase(expectedHash)
    if (hash.equalsIgnoreCase(expectedHash)) {
      logger.info(s"Download complete and verified, unzipping result")
      val cmds = Vector(
        "tar",
        "-xf",
        archiveLocation.toString,
        "--directory",
        versionDir.toString
      )
      logger.info(s"Extracting archive with command: $cmds")
      cmds.!!
    } else {
      logger.error(
        s"Downloaded invalid version of c-lightning, got $hash, expected $expectedHash")
    }

    logger.info(s"Deleting archive")
    Files.delete(archiveLocation)

    if (!success) {
      throw new RuntimeException(
        s"Failed to download and verify clightning v$version")
    }
  }
}
