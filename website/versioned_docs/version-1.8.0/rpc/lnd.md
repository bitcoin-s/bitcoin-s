---
id: version-1.8.0-lnd-rpc
title: LND
original_id: lnd-rpc
---

This is an RPC client for [LND](https://github.com/LightningNetwork/lnd). It assumes that a bitcoind instance is running.

Currently, this RPC client is written for [v0.13.3](https://github.com/lightningnetwork/lnd/releases/tag/v0.13.3-beta) version of LND.

## Configuration of LND

Please see the [sample configuration for LND](https://github.com/lightningnetwork/lnd/blob/v0.13.3-beta/sample-lnd.conf).

You can find the configuration we use for our testing infrastructure for lnd [here](https://github.com/bitcoin-s/bitcoin-s/blob/656e0928bf1bf4f511f60dec625699b454f29a1f/testkit/src/main/scala/org/bitcoins/testkit/lnd/LndRpcTestUtil.scala#L90).

## Starting LND

You need to download the binaries from the [LND's github](https://github.com/lightningnetwork/lnd/releases/tag/v0.13.3-beta).

To run lnd by unzipping the `lnd-linux-amd64-v0.13.3-beta.tar.gz` (or whichever platform you are on) and then running

```bash
$ ./lnd-linux-amd64-v0.13.3-beta/lnd
```

If you wish to start lnd from the RPC client, you can construct a [`LndRpcClient.binary`](https://github.com/bitcoin-s/bitcoin-s/blob/656e0928bf1bf4f511f60dec625699b454f29a1f/lnd-rpc/src/main/scala/org/bitcoins/lnd/rpc/LndRpcClient.scala#L35) field set

We will default to using the `binary` field first when trying to start the jar, and the fallback to the default datadir (`~/.lnd`).

Here is an example of how to start lnd:


```scala
implicit val system = ActorSystem(s"lnd-rpc-${System.currentTimeMillis}")
implicit val ec = system.dispatcher

val datadirPath = Paths.get("path", "to", "datadir")
val binaryPath = Paths.get("path", "to", "lnd-linux-amd64-v0.13.3-beta", "lnd")
val instance = LndInstanceLocal.fromDataDir(datadirPath.toFile)
val client = new LndRpcClient(instance, Some(binaryPath.toFile))

val startedF = client.start()

for {
  lnd <- startedF
  info <- lnd.getInfo
} yield {
  println(s"Lnd info: $info")
}
```

### Updating to a new LND version

The lnd rpc module uses lnd's gRPC. This means when updating to the latest version, the `.proto` files will need to be updated.
Bitcoin-S stores them in [lnd-rpc/src/main/protobuf](https://github.com/bitcoin-s/bitcoin-s/tree/master/lnd-rpc/src/main/protobuf).
You can find the files to copy from LND [here](https://github.com/lightningnetwork/lnd/tree/master/lnrpc).

After updating the `proto` files you can run `sbt compile` and this will generate the corresponding class files, this should then give
compile warnings for changed rpc functions.
