name := "bitcoin-s-server-grpc"

libraryDependencies ++= Deps.serverGrpc

CommonSettings.prodSettings

enablePlugins(PekkoGrpcPlugin)

// Suppress deprecation and unused-imports warnings from generated code
Compile / scalacOptions ++= Seq(
  "-Wconf:cat=deprecation:site=org\\.bitcoins\\.server\\.grpc\\..*:silent",
  "-Wconf:cat=unused-imports:site=org\\.bitcoins\\.server\\.grpc\\..*:silent"
)
