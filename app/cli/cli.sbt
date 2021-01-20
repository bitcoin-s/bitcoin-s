name := "bitcoin-s-cli"

libraryDependencies ++= Deps.cli(scalaVersion.value)

nativeImageOptions ++= Seq(
  "-H:+ReportExceptionStackTraces",
  "--initialize-at-build-time",
  "--no-fallback",
  "--enable-http",
  "--enable-https"
)

enablePlugins(JavaAppPackaging, NativeImagePlugin)
