name := "bitcoin-s-cli"

libraryDependencies ++= Deps.cli(scalaVersion.value)

ThisBuild / scalaVersion := "2.12.9"

graalVMNativeImageOptions ++= Seq(
  "-H:EnableURLProtocols=http",
  // builds a stand-alone image or reports a failure
  "--no-fallback",
  // without this, we get complaints about Function3
  // I'm not sure why, though...
  "--initialize-at-build-time=scala.Function3",
  "--verbose"
)

enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin)

publish / skip := true
