name := "bitcoin-s-oracle-server"

// Ensure actor system is shut down
// when server is quit
Compile / fork := true

libraryDependencies ++= Deps.oracleServer

mainClass := Some("org.bitcoins.oracle.server.OracleServerMain")

packageSummary := "A DLC Oracle"

packageDescription := "A basic DLC oracle that allows you to commit to events and sign them"

enablePlugins(JavaAppPackaging, DockerPlugin)

//https://sbt-native-packager.readthedocs.io/en/latest/formats/docker.html
dockerBaseImage := "openjdk"

packageName in Docker := packageName.value

version in Docker := version.value

dockerExposedPorts ++= Seq(9998)
