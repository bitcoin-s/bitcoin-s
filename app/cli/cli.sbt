name := "bitcoin-s-cli"

libraryDependencies ++= Deps.cli(scalaVersion.value)

nativeImageOptions ++= Seq(
  "-H:+ReportExceptionStackTraces",
  "--initialize-at-build-time",
  "--no-fallback"
)

enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin, NativeImagePlugin)
