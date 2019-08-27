name := "bitcoin-s-bitcoind-rpc-test"

libraryDependencies ++= Deps.bitcoindRpcTest(scalaVersion.value)

lazy val downloadBitcoind = taskKey[Unit] {
  "Download bitcoind binaries, extract to ./bitcoind-binaries"
}

import java.nio.file.Paths
lazy val bitcoindRpc = project in Paths.get("..", "bitcoind-rpc").toFile

Test / test := (Test / test dependsOn bitcoindRpc / downloadBitcoind).value
