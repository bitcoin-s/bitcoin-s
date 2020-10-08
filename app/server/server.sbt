name := "bitcoin-s-server"

// Ensure actor system is shut down
// when server is quit
Compile / fork := true

libraryDependencies ++= Deps.server(scalaVersion.value)

mainClass := Some("org.bitcoins.server.BitcoinSServerMain")

graalVMNativeImageOptions ++= Seq(
  "-H:EnableURLProtocols=http",
  "-H:+ReportExceptionStackTraces",
  // builds a stand-alone image or reports a failure
  "--no-fallback",
  // without this, we get complaints about Function3
  // I'm not sure why, though...
  "--initialize-at-build-time=scala.Function3",
  "--report-unsupported-elements-at-runtime",
  "--verbose"
)

packageSummary := "A Bitcoin neutrino node and wallet"

packageDescription := "Runs a Bitcoin neutrino node and wallet, has functionality " +
  "for many different modes and configuration options, see more at https://bitcoin-s.org/docs/applications/server"

enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin)
