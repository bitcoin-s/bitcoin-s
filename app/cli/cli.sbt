name := "bitcoin-s-cli"

libraryDependencies ++= Deps.cli(scalaVersion.value)

nativeImageOptions ++= Seq(
  "-H:+ReportExceptionStackTraces",
  "--initialize-at-build-time"
)

enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin, NativeImagePlugin)
