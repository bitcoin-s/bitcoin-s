name := "bitcoin-s-bitcoind-rpc-test"

libraryDependencies ++= Deps.bitcoindRpcTest(scalaVersion.value)

Test / test := (Test / test dependsOn {
  Projects.bitcoindRpc / TaskKeys.downloadBitcoind
}).value
