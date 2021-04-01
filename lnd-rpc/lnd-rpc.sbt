import java.nio.file._
import scala.util.Properties

name := "bitcoin-s-lnd-rpc"

libraryDependencies ++= Deps.lndRpc

CommonSettings.prodSettings

enablePlugins(AkkaGrpcPlugin)

// Disable deprecation warning otherwise protobuf deprecation warnings will cause errors
scalacOptions in Compile += {
  //Added to supress all of the deprecation warnings inside of the
  //generated scala code by the grpc plugin. Two packages have deprecations
  //inside of them, 'signrpc' and 'lnrpc'
  //see: https://www.scala-lang.org/2021/01/12/configuring-and-suppressing-warnings.html
  if (scalaVersion.value.startsWith("2.13")) {
    "-Wconf:cat=deprecation:site=lnrpc\\..*:silent,cat=deprecation:site=signrpc\\..*:silent"
  } else {
    //-Wconf doesn't work on Scala 2.12.x until we get 2.12.13
    //which is currently blocked on a scoverage bug
    ""
  }
}

TaskKeys.downloadLnd := {
  val logger = streams.value.log
  import scala.sys.process._

  val binaryDir = CommonSettings.binariesPath.resolve("lnd")

  if (Files.notExists(binaryDir)) {
    logger.info(s"Creating directory for lnd binaries: $binaryDir")
    Files.createDirectories(binaryDir)
  }

  val version = "0.12.1-beta"

  val (platform, suffix) =
    if (Properties.isLinux) ("linux-amd64", "tar.gz")
    else if (Properties.isMac) ("darwin-amd64", "tar.gz")
    else if (Properties.isWin) ("windows-amd64", "zip")
    else sys.error(s"Unsupported OS: ${Properties.osName}")

  logger.debug(s"(Maybe) downloading lnd binaries for version: $version")

  val versionDir = binaryDir resolve version
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

    logger.info(s"Download complete, unzipping result")

    val extractCommand = s"tar -xzf $archiveLocation --directory $binaryDir"
    logger.info(s"Extracting archive with command: $extractCommand")
    extractCommand.!!

    logger.info(s"Deleting archive")
    Files.delete(archiveLocation)
  }
}
