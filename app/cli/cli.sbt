name := s"bitcoin-s-cli-${System.getProperty("os.name")}"

libraryDependencies ++= Deps.cli.value

nativeImageOptions ++= Seq(
  "-H:+ReportExceptionStackTraces",
  "--initialize-at-build-time",
  "--no-fallback",
  "--enable-http",
  "--enable-https"
)
