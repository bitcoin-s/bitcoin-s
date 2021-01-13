name := "bitcoin-s-server"

// Ensure actor system is shut down
// when server is quit
Compile / fork := true

libraryDependencies ++= Deps.server(scalaVersion.value)

mainClass := Some("org.bitcoins.server.BitcoinSServerMain")

packageSummary := "A Bitcoin neutrino node and wallet"

packageDescription := "Runs a Bitcoin neutrino node and wallet, has functionality " +
  "for many different modes and configuration options, see more at https://bitcoin-s.org/docs/applications/server"

enablePlugins(JavaAppPackaging)
