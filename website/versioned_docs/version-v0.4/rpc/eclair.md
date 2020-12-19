---
id: version-v0.4-rpc-eclair
title: Eclair
original_id: rpc-eclair
---

This is a RPC client for [Eclair](https://github.com/acinq/eclair). It assumes that a bitcoind instance is running.

Currently this RPC client is written for [v0.4.1](https://github.com/ACINQ/eclair/releases/tag/v0.4.1) version of Eclair.

## Configuration of Eclair

Please see the configuration secion of the
[Eclair README](https://github.com/acinq/eclair#configuring-eclair).

You can find the configuration we use for our testing infrastrture for eclair [here](https://github.com/bitcoin-s/bitcoin-s/blob/a043d3858ef33da51229ee59c478d2a6c9d5a46f/testkit/src/main/scala/org/bitcoins/testkit/eclair/rpc/EclairRpcTestUtil.scala#L98).

## Starting Eclair

You need to download the jar from the [eclair's github](https://github.com/ACINQ/eclair/releases/tag/v0.4.1).

To run Eclair by unzipping the `eclair-node-0.5.0-ac08560-bin.zip` and then running

```bash
$ ./eclair-node-0.4-69c538e/bin/eclair-node.sh
```

If you wish to start Eclair from the RPC client, you can do one of the following:

1. Construct a [`EclairRpcClient.binary`](https://github.com/bitcoin-s/bitcoin-s/blob/a043d3858ef33da51229ee59c478d2a6c9d5a46f/eclair-rpc/src/main/scala/org/bitcoins/eclair/rpc/client/EclairRpcClient.scala#L51) field set
2. Set the [`ECLAIR_PATH`](https://github.com/bitcoin-s/bitcoin-s/blob/a043d3858ef33da51229ee59c478d2a6c9d5a46f/eclair-rpc/src/main/scala/org/bitcoins/eclair/rpc/client/EclairRpcClient.scala#L701) environment variable to the directory where the Eclair Jar is located.

We will default to using the `binary` field first when trying to start the jar, and the fallback to `ECLAIR_PATH`.

Here is an example of how to start eclair:


```scala
implicit val system = ActorSystem(s"eclair-rpc-${System.currentTimeMillis}")
implicit val ec = system.dispatcher

val datadirPath = Paths.get("path", "to", "datadir")
val binaryPath = Paths.get("path", "to", "eclair-node-0.3.3-12ac145.jar")
val instance = EclairInstance.fromDatadir(datadirPath.toFile,None)
val client = new EclairRpcClient(instance, Some(binaryPath.toFile))

val startedF = client.start()

for {
  eclair <- startedF
  info <- eclair.getInfo
} yield {
  println(s"Eclair info: $info")
}
```

### Connecting to the websocket

As of `v0.3.3` eclair supports a websocket endpoint. This means you can receive updates of what is happening with eclair
in real time. You can see an example of us testing this [here](https://github.com/bitcoin-s/bitcoin-s/blob/a043d3858ef33da51229ee59c478d2a6c9d5a46f/eclair-rpc-test/src/test/scala/org/bitcoins/eclair/rpc/EclairRpcClientTest.scala#L591)
