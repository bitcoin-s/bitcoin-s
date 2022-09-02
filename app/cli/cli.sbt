name := s"bitcoin-s-cli"

Universal / packageName := CommonSettings.buildPackageName((Universal /packageName).value, true)

libraryDependencies ++= Deps.cli.value

nativeImageOptions ++= Seq(
  "-H:+ReportExceptionStackTraces",
  "--initialize-at-build-time",
  "--no-fallback",
  "--enable-http",
  "--enable-https"
)
