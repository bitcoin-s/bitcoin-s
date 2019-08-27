---
id: rpc-eclair
title: Eclair
---

This is a RPC client for [Eclair](https://github.com/acinq/eclair). It assumes that a bitcoind instance is running.

Currently this RPC client is written for [v0.2-beta8](https://github.com/ACINQ/eclair/releases/tag/v0.2-beta8) version of Eclair.

## Configuration of Eclair

Please see the configuration secion of the
[Eclair README](https://github.com/acinq/eclair#configuring-eclair).

## Starting the jar

You need to download the jar from the [Eclair GitHub](https://github.com/ACINQ/eclair/releases/tag/v0.2-beta8).

To run Eclair you can use this command:

```bash
$ java -jar eclair-node-0.2-beta8-52821b8.jar &
```

If you wish to start Eclair from the RPC client, you can do one of the following:

1. Construct a `EclairRpcClient` with the `binary` field set
2. Set the `ECLAIR_PATH` environment variable to the directory where the Eclair Jar is located.
