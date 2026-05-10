name := "bitcoin-s-server-grpc"

libraryDependencies ++= Deps.serverGrpc

CommonSettings.prodSettings

enablePlugins(PekkoGrpcPlugin)

Compile / scalacOptions ++= Seq(
  "-Wconf:cat=deprecation:site=org\\.bitcoins\\.server\\.grpc\\..*:silent",
  "-Wconf:cat=unused-imports:site=org\\.bitcoins\\.server\\.grpc\\..*:silent"
)
