# Bitcoin-s Eclair RPC client

This is a RPC client for [eclair](https://github.com/acinq/eclair)

Currently this RPC client is written for the latest official version of eclair which is [v0.2-beta8](https://github.com/ACINQ/eclair/releases/tag/v0.2-beta8)

## Configuration eclair 

Please see this section of the eclair README

https://github.com/acinq/eclair#configuring-eclair


## Starting the jar 

You need to download the jar from this link. 

https://github.com/ACINQ/eclair/releases/tag/v0.2-beta8

to run eclair you can use this command 

```

$ java -jar eclair-node-0.2-beta8-52821b8.jar &

```

alternatively you can set the `ECLAIR_PATH` env variable and then you can start Eclair with the `start` method on `EclairRpcClient`. 

**YOU NEED TO SET `ECLAIR_PATH` CORRECTLY TO BE ABLE TO RUN THE UNIT TESTS**
