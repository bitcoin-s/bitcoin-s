name := "bitcoin-s-cli-grpc"

Universal / packageName := CommonSettings.buildPackageName((Universal / packageName).value)

libraryDependencies ++= Deps.cliGrpc

mainClass := Some("org.bitcoins.cli.grpc.CliGrpc")
