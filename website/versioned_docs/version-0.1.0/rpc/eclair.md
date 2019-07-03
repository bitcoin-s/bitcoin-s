---
id: version-0.1.0-rpc-eclair
title: Eclair
original_id: rpc-eclair
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

Alternatively you can set the `ECLAIR_PATH` env variable and then you can start Eclair with the `start` method on `EclairRpcClient`.

**YOU NEED TO SET `ECLAIR_PATH` CORRECTLY TO BE ABLE TO RUN THE UNIT TESTS**
