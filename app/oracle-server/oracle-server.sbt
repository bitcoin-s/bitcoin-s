name := "bitcoin-s-oracle-server"

// Ensure actor system is shut down
// when server is quit
Compile / fork := true

libraryDependencies ++= Deps.oracleServer

mainClass := Some("org.bitcoins.oracle.server.Main")

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

packageSummary := "A DLC Oracle"

packageDescription := "A basic DLC oracle that allows you to commit to events and sign them"

enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin)
