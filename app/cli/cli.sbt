name := "bitcoin-s-cli-schnorr"

libraryDependencies ++= Deps.cli(scalaVersion.value)

graalVMNativeImageOptions ++= Seq(
  "-H:EnableURLProtocols=http",
  "-H:+ReportExceptionStackTraces",
  // builds a stand-alone image or reports a failure
  "--no-fallback",
  // without this, we get complaints about Function3
  // I'm not sure why, though...
  "--initialize-at-build-time=scala.Function3",
  "--report-unsupported-elements-at-runtime",
  "--verbose",
  "--allow-incomplete-classpath"
)

enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin)

publish / skip := true
