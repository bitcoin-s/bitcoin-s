# 1.9.8

This release is backwards compatible with the 1.9.x series of bitcoin-s

See the individual module sections for more information on lower level updates to the codebase.

Want to get started quickly? See our `docker-compose.yml` file. [See instructions here](https://github.com/bitcoin-s/bitcoin-s/#docker)

If you are a typescript developer, [you can access the backend via our typescript library](https://github.com/bitcoin-s/bitcoin-s-ts)

# Executive Summary

This release reworks the `node` implementation to use pekko streams. This release of bitcoin-s
also moves away from akka to pekko due to akka's licensing changes.

## Running bitcoin-s

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-1.9.8.zip` and then run it with `chmod +x ./bin/bitcoin-s-server && ./bin/bitcoin-s-server` to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-1.9.8.zip` folder and start using the `bitcoin-s-cli` like this:

```bashrc
./bin/bitcoin-s-cli --help
Usage: bitcoin-s-cli [options] [<cmd>]

  -n, --network <value>    Select the active network.
  --debug                  Print debugging information
  --rpcport <value>        The port to send our rpc request to on the server
  -h, --help               Display this help message and exit
```

For more information on what commands `bitcoin-s-cli` supports check the documentation, here is where to
start: https://bitcoin-s.org/docs/next/applications/server#server-endpoints

## Verifying signatures

This release is signed with [Chris's signing key](https://bitcoin-s.org/docs/next/security#disclosure) with
fingerprint `9234F4D6AF47C71B741A390F8976CA0AF71A7A2A`

To do the verification, first hash the executable using `sha256sum`. You should check that the result is listed in
the `SHA256SUMS.asc` file next to its file name. After doing that you can use `gpg --verify` to authenticate the
signature.

Example:

```
$ gpg -d SHA256SUMS.asc > SHA256SUMS.stripped
gpg: Signature made Mon 18 Apr 2022 02:19:54 PM CDT
gpg:                using RSA key 9234F4D6AF47C71B741A390F8976CA0AF71A7A2A
gpg: Good signature from "Chris Stewart <stewart.chris1234@gmail.com>" [ultimate]

$ sha256sum -c SHA256SUMS.stripped

```

### Website

https://bitcoin-s.org/

### Releases

https://repo1.maven.org/maven2/org/bitcoin-s/

### Snapshot releases

https://oss.sonatype.org/content/repositories/snapshots/org/bitcoin-s/

# Modules

## app commons

c6524b9246 remove usages of new URL() as constructor is deprecated on jdk 20+ (#5219)

## App server

Implements a `getconnectioncount` RPC to query how many peers we are connected to.

4501e138a4 Stop BitcoinSAppConfig last in BitcoinSServerMain.stop() (#5493)
b25cc8a8a5e Fix start rescan race condition, previously we would attempt to fetch blocks before our NodeApi had any peers (#5491)
00c84a9c25 Set max-open-requests = 64 for appServerTest (#5442)
c407c2e874 2024 02 29 cleanup logback files (#5441)
b0cf3d8146 Fix race condition between `NodeAppConfig.migrate()` calls when `bitcoin-s.tor.enabled=false` (#5120)
4fd7af04ca Remove check to see if seed exists when loading a wallet, the key manager can initiate the seed if it dne (#5094)
447c6d03de Implement `getconnectioncount` rpc (#5048)
6b03133a34 Fix `empty.tail` exception occurring on chain block filter / filter headers callbacks (#5021)
e3e1c7f949 Try bumping timeout to 1 second to avoid false positives test failures (#5018)
e993335f03 Improve error logging on server (#4896)

## bitcoind rpc

Adds support for bitcoind v24. Removes support for bitcoind v19 and v20 as they are past EOL.

a18bd70369 Upgrade to bitcoind `24.2` (#5284)
fa34cb4f4d Bump bitcoind minor versions to `24.1`/`23.2` (#5088)
3db663a387 Remove `v20` bitcoind support (#4992)
af349d2179 Remove support for `v19` of bitcoind (#4991)
ff8376ceb6 Call `syncwithvalidationinterfacequeue` inside of `{generatetoaddress,generateblock}` (#4987)
9a80644509 Fix UTXORpcTest flakiness (#4986)
b329c3670e Add rpc for syncwithvalidationinterfacequeue, use it in NodeUnitTest.syncNeutrinoNode (#4983)
332893b67b Add generateblock bitcoind rpc (#4906)
f4244d7a0e Add bitcoind v24 (#4902)

## Build

0bd59789e30 Remove neutrino.suredbits.com:8333 from docker-compose.yml (#5492)
58adf4c6fd3 Fix build warnings for --compress flag in jlink (#5485)
9aa730dd0a Revert yml files (#5449)
dd7131306c Consolidate disabled secp256k1 workflow into Compile workflow (#5445)
1317adfdad Add zmq/test and chainTest/test to OS's we weren't previously running on (#5443)
1b25994fc2 Re-add java.sql module to cli jlink image to fix #5411 (#5412)
83a27e2775 Add new macOS CI matrix to try and speed up mac jobs (#5364)
db21b8873e Set flag nativeImageInstalled =: true so we don't overwrite system's nativeimage (#5358)
a7b67fc78d 2024 01 18 fix native image build (#5357)
16fb5d2dad 2023 12 19 compiler opts (#5330)
1084be80ad Upgrade cache action to v3 (#5326)
a93954cc4e Only generate scaladoc on CI to decrease build time (#5220)
a18dc6c55f Add java.instrument to jlink dev builds (#5210)
dd77eb0566 Add jdk.management.agent module on dev builds (#5115)
cb272ddf83 Fix build by ignoring junit transitive dep (#5071)

## chain

Fixes a few bugs with filter's and reorgs.

8b23b1f4f6 Add delay to wait for zmq to get setup in chainTest (#5427)
8f812c644f Fix Promise already completed errors in chainTest (#5440)
748121fe8a 2023 01 06 small chainhandler bug fixes (#5345)
3a927edee1 Fix bug when requesting `FilterSyncMarker` for filter headers (#5344)
5e81ec5ed2 Add `ChainApi.getBestChainTips()` (#5338)
f1775c46d3 Simplify ChainHandler.nextFilterHeaderBatchRange() (#5336)
af361167a4 Refactor `ChainHandler.findNextHeader()` to only take 1 blockchain (#5335)
d983a1bac4 2023 12 24 Make filter headers / filters sync work with block header reorgs (#5333)
3e6ff52194 2023 09 27 issue 5139 (#5247)
e15bf6cc9d reduce default `bitcoin-s.chain.neutrino.filter-batch-size` to `250` (#5230)
0e2fddcc57 Optimize bestFilterHeaderHeightQuery (#5223)
8ab9718a48 Reduce best filter height sql query costliness by using a cheap check (#5206)
39cec3ae51 Fix bug where we weren't checking if filters are stale when determing if we need to sync filters (#5203)
4a344f0187 Fix bug in `ChainApi.processFilterHeaders()` (#5157)
dc0e3645d0 2023 03 24 is tip stale (#5024)
de0e892b3e 2023 02 27 Fix bug where duplicate filters caused node not to sync (#5008)
7f08458a55 Check compact filter headers if we have already stored them in the database (#5003)

## cli 

c038d2fa3b 2023 08 30 refactor `ConsoleCli` (#5217)
d1c9228361 Fix getpeers -> getconnectioncount in cli (#5052)

## clightning rpc

d53ed7d487 Bump clightning version to `v23.02.2` (#5025)

## Core

Fixes parsing bugs, caches txid computation to avoid re-computing large transaction's txid.

55e4dab86e Rework `NodeApi.downloadBlocks()` to use `DoubleSha256DigestBE`  (#5456)
b27cebca1c 2024 02 23 `TaprootKeyPath.isValid()` bug (#5421)
195cfbd273 Fix handling invoice with minimal encoding of feature bits (#5405)
d641c3da55 2024 01 25 Cache txid (#5374)
5885f4e99e 2024 01 26 rm lazy bytes (#5373)
be1ec842c2 Fix bug in classifying things as `MultiSigSPK` that do not have enough public keys in the Script (#5371)
a66925dba0 Fix bug where IndexOutOfBoundsException was occuring when parsing a script with only OP_CMS (#5370)
fd09724f48 2024 01 23 Validate we have `maxSigs` number of public keys in `MultiSignatureScriptPubKey` (#5369)
039144c2c8 Add access to underlying MultiSignatureScriptPubKey scriptNumbers represents requiredSigs, maxSigs (#5366)
0f95a1f7bb Fix bounds checking for MultiSignatureScriptPubKey.maxSigsRequired (#5365)
73785706d8 2024 01 20 Fix bug in parsing `OP_CLTV` and `OP_CSV` (#5363)
618e1ca2d2 Fix bug in script parsing logic where we weren't checking if a ScriptNumber was negative for a MultiSignatureScriptPubKey (#5362)
421970dcf5 2024 01 11 bip68 bip112 txversion bug (#5346)
522821869d Move filterBatchCache into NodeState.FilterSync (#5253)
4972d0a368 Move NodeState to core (#5249)
99ca1b7abf Add PeerManagerApi.peers (#5170)
ef2c96cc99 Add PeerManagerApi (#5164)
ba8a0bf75a 2023 07 17 coretest script refactor (#5150)
b1e6488bb7 Move Socks5ProxyParams, Credentials into core (#5138)
aa8da307b7 Remove `batchAndParallelExecute` test case (#5122)
3740b4fc35 Implement batchAndParallelExecute with Future.traverse rather than Future.sequence (#5051)
3728b9a9d9 Refactor `HDPath` pattern matching to be safer. (#5046)
5e6cb0d514 Fix MilliSatoshis.toSatoshis for large values (#5016)
0577b8c1fe Improve Inventory toString messages (#4997)
8bd10d0f40 Correctly handle witness v1+ spks in fallback addresses (#4949)
7a3c6aef36 Fix isLocalhost for 127.0.0.1 (#4900)

## Crypto

## db commons

c92f2ed009 Add db mapper for ECDigitalSignature (#5002)

## DLC node

2b117d349a Remove duplicate Peer class (#5141)

## DLC Oracle

## DLC Server

556c5e0c39 Fix NullPointerException inside of DLCServer (#5287)

## DLC wallet

## Eclair rpc

2799d8276a Update Eclair RPC to v0.8.0 (#4994)

## gui

## fee rate

## keymanager

## Lnd rpc

f01987160d Lnd v0.17.3 (#5375)
008dd42a08 Upgrade lnd to `0.16.4` (#5194)
08757d536f Lnd 0.16.0 rpc (#5005)

## Lnurl

915af37ac9 Add ability to add arbitrary query string params for lnurl pays (#4995)
57b4421f8a Add nostr zaps params for lnurl (#4988)
ffee1a804e Check lnurl pay invoice description hash (#4931)
f8247c427d Add LNURL writers (#4927)

## node

Reworks the node implementation to use pekko streams. `NodeState` is now tracked
internally in a stateless manner inside the stream.

9fef2c505c2 Add `bitcoin-s.node.connection-attempt-cool-down-period` (#5489)
7e2a4c97e64 Reduce requesting next cfheaders log level to DEBUG (#5490)
47c7680d8c2 Refactor `Node.chainApiFromDb()` to return `ChainApi` (#5484)
3cfe700fad5 Reduce stream failure log to DEBUG to avoid tor spam (#5483)
3cfe700fad Reduce stream failure log to DEBUG to avoid tor spam (#5483)
a875f08d2e Reduce log level of PeerConnection logs to DEBUG (#5481)
af342dd6c1 2024 03 18 rm unused configs (#5480)
d44344f07b Check if peer is waitingForDisconnection when receiving InitializeDisconnect() message (#5479)
f7dd69bec3 refactor: Use 'peerWithServicesDataMap' name consistently across NodeState and PeerManager (#5478)
db64a6d136 Reduce various log levels to DEBUG (#5477)
5c247e65a5 Reduce 'Attempting to connect' log to DEBUG (#5476)
9a71a96fb8 2024 03 11 Remove filtersync job (#5471)
23337b7137 2024 03 11 `PeerFinder.getPeersFromDnsSeeds` async (#5472)
708ce41f24 Re-implement `bitcoin-s.node.query-wait-time` (#5470)
54a356b300 Don't unnecessarily create new NodeState objects, use helper methods to state transition (#5469)
fa6cb1ba3a Send p2p messages to syncPeer when we are in  and a specific peer is not set in SendToPeer (#5468)
9e50c5fd12 Fix bug where we weren't checking if isFiltersSynced is true before sending our next getcfilters to our peer (#5466)
16aeb56df5 Remove implicit NodeAppConfig from PeerMessageSender (#5465)
567c2400ca Small refactors to use RunningNodeState methods, improve logs (#5463)
5967caeca7 2024 03 07 Add `NodeState.FilterOrFilterHeaderSync`, refactor `PeerManager` to use it (#5459)
e793c53dd7 Add guards on sending blockfilter messages to non-blockfilter peers (#5457)
898d2b44ee Fix bug where we were executing block callbacks when we hadn't seen a block header (#5455)
238948e185 Remove PeerMessageSenderApi param from DataMessageHandler (#5453)
ddb6f01d65 Simplify / reduce log level for compact filter logs (#5452)
367285d9b9 Reduce reliance of `PeerManager._peerDataMap` in favor of `NodeState.peerDataMap` (#5451)
0d2e0a98f0 fix bug where we weren't properly transferring state to FilterHeaderSync, reduce log level of peer we fail to connect to (#5450)
ae3a634703 2024 03 01 Fix `syncPeer` exceptions (#5446)
1f0af696f6 Reduce DataMessageHandler log levels (#5447)
951bdc5b34 Always try to attempt to connect to our param peers in PeerFinder (#5444)
8fb62ad21c Use `NodeCallbackStreamManager` when calling `NodeCallbacks` constructors (#5439)
70f983213c Ignore invs while in IBD rather than when syncing (#5433)
d1ecd35e63 Fix `PeerDb.lastSeen` race condition in unit tests (#5435)
ab0b0e2209 Fix bug where we weren't removing peer from `NodeRunningState.waitingForDisconnection` (#5425)
0053ccd853 Rework NodeState.toString() (#5432)
8a0c00637c Remove duplicate `PeerConnection.disconnect()` call (#5426)
6934b33474 Fix bug in health checks were we wouldn't attempt to connect NodeAppConfig.maxConnectedPeers if we didn't have any non compact filter peers connected (#5422)
90b9e76e56 Keeping finding peers until all peers are blockfilter peers (#5417)
783ed1903d Rename bitcoin-s.node.inactivity-timeout -> bitcoin-s.node.health-check-interval (#5418)
3177ee405f Move away from suredbits provided peers by default (#5390)
b89b31fe09 2024 02 18 peerfinder duplicate connect (#5403)
155b4fbc76 Fix case were we weren't cancelling the syncFilter job onDisconnect() (#5402)
f5087c5e3f Remove PeerManager.{getPeerMessageSender(), getPeerConnection()}, use NodeState.{getPeerMessageSender(), getPeerConnection()} (#5400)
e4fbfe73e6 2024 02 16 fix get filter sync marker from stop block header bug (#5401)
bc94a8b01f 2024 02 15 Fix duplicate sync bug when we have a misbehaving peer (#5399)
fe33c2919c Fix inconsistency of `PeerDAO` primary key usage (#5398)
536cc26ba5 Update last seen value in PeerDb on disconnect (#5397)
05d03e7da6 Remove skipPeers, better PeerFinder.toString() (#5396)
2d3e481ce0 Use Future.successful() when removing peer as there is no underlying async computation (#5394)
9d58c9eb89 Implement health checks for peers, first check is if we have any blockfilter peers (#5393)
d4ae659887 Log instead of throwing exception when sending message to peer without an active connection (#5392)
cac546bb57 Fix bug where we were only setting `bitcoin-s.node.inactivity-timeout` on regtest (#5389)
451e019225 2024 01 31 Fix `PeerManager.connect()` bug where `PeerFinder` was unaware of peer (#5381)
75881191d1 Guard NeutrinoNode.start() with isStarted flag (#5380)
75f6a3b4ec Refactor PeerFinder.peerConnectionScheduler() into method (#5377)
76e468c5c4 2024 01 28 Refactor `PeerFinder.start()` to avoid initializing connections (#5376)
f26e6f7c5a initiate disconnect from PeerManager rather than bitcoind in connection (#5354)
9cd60d5366 2024 01 02 encapsulate state p2psink (#5342)
56b1a557a8 Add carve out to ignore ConnectPeer request if state is NodeShuttingDown (#5352)
3c5bace825 Dont initiate disconnect logic from bitcoind, its flakey for some reason (#5343)
d6c1491ba8 2024 01 03 shuttingdown nodestate (#5341)
30876c2cde Move `PeerManager.connectPeer()` logic into stream (#5340)
d27dcb38f4 Add logging to PeerConnection.handleStreamComplete so exceptions aren't dropping on the floor (#5339)
a3e1267484 2023 12 30 isfiltersync bug (#5337)
06dfd9cea4 Rework `randomPeerWithServices()` to be inside of `NodeState` (#5331)
0ace6dfd2e Make `PeerManager.gossipMessage()` queue to avoid gossiping messages to peers being disconnected (#5334)
54f303efb0 Rework various private methods in `DataMessagehandler` to return `NodeState` (#5329)
ce154fc5fd Remove unecessary helper method in `PeerFinder` (#5327)
747389e77d Rework `ConnectionGraph.stop()` to return `Future[Done]` (#5325)
01537185bd 2023 12 13 mv connectioncount check fixtures (#5324)
7f337073c9 Improve `ReConnectionTest` (#5323)
f4db40897b Fix bug where `PeerConnection.connectionGraphOpt` does not get reset on disconnect (#5307)
bd3ad1df21 2023 12 07 mat socks5handler (#5322)
47c433f365 Replace inactivity logic with `Flow.idleTimeout` (#5311)
16c41c3b2e Added LnURLClient accept header to request (#5318)
4913b12431 2023 11 10 peermanager refactor (#5303)
7f5ed15521 Reduce inactivity level to `DEBUG` (#5286)
7b8df425fb Reset `PeerConnection.connectionGraphOpt = None` when connection is disconnected (#5290)
be30da578d Bump timeout for checking when peer is disconnected (#5277)
cff757cb55 2023 10 03 node refactors (#5256)
2863b3f5cc Change PeerManager to keep NodeState in Sink.foldAsync() rather than DataMessageHandler (#5255)
a40be1a6bc Fix bug where we were executing block ehader callbacks when we received 0 block headers from our peer (#5254)
1602852419 2023 09 29 waitingdisconnection nodestate (#5251)
83cf657a0b Move initialization of sync into PeerManager's queue (#5250)
c4f0bd3d39 Switch defaults to 12 hours for running peerConnectionScheduler (#5241)
3763a53ad5 Adjust so that requery in the case were we have just 1 peer (#5240)
d5c74c086d Reduce NodeCallbackStreamManager from 32 -> 16 (#5229)
b3aa7b4b68 Fix bug were we weren't consistently calculating the correct compact filter start height for sync (#5225)
489682312d Reduce NodeCallbackStreamManager to 32 to reduce chance of OOM errors on small heap sizes (#5224)
3ae41032a4 Add log to help debug 5208 (#5211)
aa02683db9 Cleanup use of syncPeerOpt as a param in parts of PeerManager (#5209)
2672c2223c Remove references to client in PeerManager logs (#5207)
11ab921923 Add filter header and filter count to logs (#5202)
26290bf4c0 Rework handling of `BlockMessage`, fix bug where callbacks executed when they shouldn't have been (#5201)
e8ab293fa0 Remove `Initialized` from `NodeStreamMessage`, return `Initialized `from `ControlMessageHandler` (#5199)
cc94a92424 Add Await.result() to inactivity check runnable to make sure it completes, or we get an error message if it doesn't (#5197)
f36c437648 Guard createFilterSyncJob() with isStarted (#5195)
e423de01e1 Remove executing block callbacks if node is in IBD (#5193)
998c997ac5 Move initialization cancellable into connection graph  (#5192)
deb34dc87a Lower log levels to DEBUG (#5188)
09623173b3 Increase inactivity check log level (#5187)
f2b40c8922 More idiomatic akka stream usage (#5183)
b13e0565af Remove `PeerMessageReceiver` (#5182)
c238191209 2023 08 04 rm peermsgrecvstate (#5177)
25f42bf6b8 Reduce log level of 'Received maximum amount of headers' (#5181)
08e780a884 2023 08 01 issue 5174 (#5176)
147f7782e5 2023 07 28 cleanup tests (#5172)
9b85838823 Implement logic to restart `PeerManager` in inactivity checks when we have 0 peers (#5171)
4d5488f193 2023 07 31 fix filterheader sync bug (#5173)
cade1afcdf Implement more checks for createFilterSyncJob() to more strictly check if we have seen any filter headers / filters since the job was scheduled (#5168)
e570ebea56 Remove validating headers state (#5165)
c2902c1146 Fix `onBlockReceived` unit test (#5166)
2d57ff6a3e Refactor `PeerManager` methods to be `connectPeer()`, `disconnectPeer()` (#5163)
4afaaf8f22 Only log a warning instead of throwing an exception when we try to send a message to a peer we aren't connected to (#5161)
878dce7945 Use Await.result() in DataMessageHandlerTest to avoid test cases that never complete (#5159)
2032b16620 Fix bug where we just need to `awaitSyncAndIBD()` rather than attempt to `sync()` (#5158)
1ccc6a9230 2023 07 19 cleanup nodetest (#5156)
e08469901b Make `PeerMessageSender.reconnect()` return a `Future` that is completed when the connection is established (#5155)
44190a535c Defensively try to sync everytime a peer is a initialized (#5154)
f034435430 Move setSyncing into PeerManager.syncHelper() (#5153)
e66c078863 First attempt at implementing `inactivityChecks()` (#5144)
4bd5616e67 Make sure `PeerData.stop()` is called  when `PeerFinder.removePeer()` is called (#5147)
ebe79287af Implement socks5 proxy in PeerMessageSender (#5136)
ee619051a3 2023 07 04 node refactors (#5130)
9b6bca06c0 Fix duplicate filter header sync by adding delay before attempting to sync filter headers (#5132)
fc99087c89 Ignore messages in queue that were queued before we disconnected peer (#5131)
5e6caa866f Implement call to Node.stop() when shutting down BitcoinSServerMain (#5124)
f522ffec72 2023 06 28 connection fail logs (#5121)
6befad2dd3 Remove `offer(SendToPeer)` from `PeerManager` queue (#5119)
08a76fb040 Fix bug in `DataMessageHandler.isFiltersSynced()` (#5118)
1d82ed04a7 2023 06 18 implement tcp connection using akka streams (#5111)
5ae4993bed Move `PeerMessageReceiverState` into `PeerMessageReceiver` (#5110)
e0c9500770 Reduce p2pclient timeout to 10 seconds (#5109)
30b06b1131 Simplify `DataMessageHandlerTest` so we don't have duplicate callback execution (#5108)
c3eed1e92b 2023 05 05 Make `DataMessageHandler` be accummulated in our akka stream (#5098)
dae8c0fc9c Don't execute onResponseTimeout() when disconnecting (#5104)
f4f45a1cad Move {`syncFromNewPeer()`, `syncHelper()`} into PeerManager, remove reference to `Node` inside of `PeerManager` (#5102)
bed670fb6f Remove `chainApi` parameter from `NeutrinoNode` (#5101)
e3f8eb2cc6 Add `PeerMessageSenderApi.gossipGetHeadersMessage()`, use it in `Node.sync()` (#5100)
a5778948cb Remove downloadBlocksBasedOnIBD() as it is redundant (#5099)
ae8c97a4d1 Rework Node.sync() to return Future[Peer] rather than Future[Option[Peer]] (#5096)
295be36d63 Get PeerMessageSenderApi using akka streams for outbound p2p messages (#5069)
abeaaa05de Remove `awaitPeerWithServices()` (#5093)
e74af8bf24 Move `P2PClientCallbacks.onStop()` into disconnection logic rather than `Actor.postStop()` (#5089)
61e142a631 2023 05 29 peermanager peerfinder refactor (#5086)
6c38a791d7 Drain data message stream before `PeerManager.stop()` is complete (#5085)
9202e63c90 Create P2PClientCallbacks to encapsulate callbacks (#5084)
34df4ccbb1 Make `ControlMessageHandler` take `PeerManager` rather than `Node` as a param (#5081)
d33f17f0d7 Cancel oninit timeout scheduled job when we disconnect (#5076)
777743989a Remove `PeerHandler` as it isn't used in src (#5075)
8e4aa49aef Fix `ReConnectionTest` part 2 (#5074)
c0403da7c6 Make PeerFinder.getData() return Option (#5073)
5a8576a057 Encapsulate access to akka streams queue to `PeerManager` (#5070)
8dfb7d091f 2023 05 05 encapsulate peermanager peermsgsender (#5066)
4c6090207a Add `MisbehavingPeer` state (#5065)
2fdd237e20 2023 04 26 dmh state refactor (#5062)
ce6f0d1507 Refactor HeadersMessage to have helper method for HeadersMessage (#5060)
1fc6edf825 Remove initialSyncDone Promise as it isn't used (#5058)
ce6d2212c1 2023 04 22 peermanager dmh refactor (#5057)
1461782865 Move onHeaderRequestTimeout to PeerManager (#5056)
7beed5a00d Only request syncing compact filter headers / filters if our tip isn't stale (#5055)
18482c7e44 2023 04 20 decouple node (#5049)
5f9c89820f Add wait before checking connection count test (#5050)
51429a7d68 2023 04 17 refactor peer message receiver (#5045)
13e5e6501c Fix bug where compact filters weren't being processed in order of block height during IBD (#5041)
f95360f8ba Add logic to fetch block headers after compact filters are synced in IBD to avoid a stale tip (#5037)
31e1bd79e9 Speed up DataMessageHandlerTest dramatically by using CachedBitcoind (#5038)
e791932f99 Refactor peerData -> peerDataMap, try to fix #4955 (#5030)
54b47152d1 Fix bug fetching compact filter height to early in DataMessageHandler (#5031)
1d611ec6b5 Remove fetch compact filters in NeutrinoNode.sync() before fetching block headers / compact filter headers (#5023)
df5472f263 Generate blocks in two rpc calls (#5011)
875a67a73c 2023 03 03 rm `DataMessageHandler` filter params (#5009)
58fbcf41e3 Bump timeout in P2PClientActorTest (#4985)
56ae02918c 2022 12 05 issue 4904 (#4917)
c7febc1ef0 Segregate `P2PClientTest` and `P2PClientActorTest` (#4974)
f6207b1c9f Add check we have more than 1 peer before we try to `syncFromNewPeer()` (#4966)
3bc89c680f Fix case where syncCompactFilters is called but filters but no sync peer is set (#4965)
c5983730bb 2023 01 10 refactor peermanager (#4950)
60907a7c17 2023 01 25 mv handling network received msg (#4961)
dd08dedb41 2023 01 24 small changes from pr 4950 (#4958)
dfe92d2ba4 Remove expectTimeout Await.result() (#4957)
4e4c6d03f1 Remove Await.result() in initializationTimeoutCancellable (#4956)
9646994a99 Encapsulate PeerData.client (#4944)
3a8eff8feb Some cleanups in the node project (#4941)
8f4ed4ac83 Refactor to use NodeTestUtil.awaitSync() (#4942)
09d53460a1 Implement ability to cancel background task for querying peer with specific services (#4937)
a7930657f9 Rework logging so that stack traces aren't printed when we receive duplicate data over the p2p network (#4936)
e7dd96468d 2022 12 29 issue 4933 (#4935)
746e23cf86 Refactor NodeTestUtil.awaitSync() to check compact filter headers / compact filters as well (#4934)
bd79ab0b73 2022 12 12 rm dmh params (#4925)
96392edde7 Stop P2PClientActor when we receive Tcp.{ErrorClosed,ConfirmedClosed,Closed,Aborted,PeerClosed} commands (#4926)
0d76a06331 Call `handleDataPayloadHelper` explicitly on `DataMessageHandlerState` (#4921)
6293d45a37 Fix synced flag but in `DataMessageHandler` bug (#4920)
aab2b274f0 Remove default parameters from DataMessageHandler (#4918)
f2be536211 Delay querying for peers for 30 minutes (#4897)

## Oracle Explorer Client

c0e8d376eb Remove oracle explorer client (#5308)

## wallet

320773a99c Add `OverflowStrategy.backpressure` on `Source.queue` in bitcoind polling job (#5437)
42e5e87350 Reduce various logs in org.bitcoins.wallet to DEBUG (#5434)
af74450c62 Cancel polling job after walletTest is executed (#5379)
5c2c8ee30b 2023 10 19 recursive rescan state (#5269)
b252c2d6a2 2023 10 16 Implement `WalletCallbackStreamManager,` `DLCWalletCallbackStreamManager` (#5263)
36f30c5915 Fix bug where `bip39Password` was not passed as param in test case (#5040)
36ec40dfa3 implement generating addresses when wallet is initialized if creationTime is > 1 hour ago (#5034)
d297311814 Catch non RescanTerminatedEarly errors when rescan is terminated with an exception (#4939)
24c6dc2cdb Download blocks from random peer if we aren't in IBD (#4924)

## testkit-core

deb79c5994 Make another instance of `Gen[ECPrivateKey]` be deterministic (#5353)

## testkit

1685c84eea 2024 01 31 Refactor `BitcoindRpcTestUtil.{startSevers(),stopSevers()}` to use `Future.traverse()` (#5382)
018631dad4 Create NodeTestUtil.awaitConnectionCount() (#5378)
b39736fb8d Rework `NodeTestUtil` to use a specific `bestBlockHash` (#5332)
579318cad0 Make `Gen[ECPrivateKey]` in scalacheck to use randomness from scalacheck to preserve reproducibility of test cases (#5285)
531c909597 Use BitcoindRpcBackendUtil.buildBitcoindNodeApi() in wallet test fixtures, re-implement getConfirmationsForBlocks() to use akka streams to avoid max-open-requests limit inside of akka (#5259)
12e5dfc021 Fix chain fixtures to use `ChainHandler` rather than always using `ChainHandlerCached` (#5243)
458a80d854 Destroy wallet config when tearing down node (#5105)
7e346e58fe Refactor `NodeTestUtil.awaitBestHash()` to take a reference to `bitcoind` (#4973)
1391c9497e Segregate handling of exceptions in fixtures (#4971)
76fc3cdc41 Refactor to not throw an exception is node is already syncing (#4970)
60998fe2d1 Reduce timeouts for syncing in NodeTestUtil (#4923)

## tor

ef20d5ec83 Rework Socks5Connection.socks5Handler() to emit Socks5ConnectionState downstream (#5315)

## Website

f8143f3e02 init 1.9.8 release notes

## ZMQ

cb26f6de07 Add WalletZmqSubscribers (#5423)

## Dependencies

c51c6a8f391 Update sbt-buildinfo to 0.12.0 (#5486)
a2b354c6943 Update client3:core, ... to 3.9.5 (#5487)
1e0cd1bed39 Update sbt-scalajs, scalajs-compiler, ... to 1.16.0 (#5488)
177542a13c Remove grizzled.sl4fj was its no longer maintained (#5482)
d1ba4737d6 Update sbt-assembly to 2.2.0 (#5473)
270f3cf624 Update postgresql to 42.7.3 (#5475)
b7d7baaa20 Update sqlite-jdbc to 3.45.2.0 (#5474)
dcb177b4a7 Update slick, slick-hikaricp to 3.5.0 (#5461)
c2fb5d2657 Update client3:core, ... to 3.9.4 (#5460)
f77b4efe11 Update logback-classic to 1.5.3 (#5448)
5ab8da9adc Update logback-classic to 1.5.1 (#5438)
87003eb703 Update sbt-scalafmt to 2.5.2 (#5218)
3ed78b5270 Upgrade to scala 2.13.13, fix compiler errors (#5428)
dae6ebf9aa Update sbt-scoverage to 2.0.11 (#5424)
3c2184156b Update sbt to 1.9.9 (#5419)
0a96f72c5e Update postgresql to 42.7.2 (#5414)
1dff918848 2023 02 21 pekko (#5413)
caeb8d559c Update sbt-scoverage to 2.0.10 (#5409)
1efede0401 Update junixsocket-core to 2.9.0 (#5407)
b059c2f57e Update jeromq to 0.6.0 (#5410)
7c8acc4484 Update client3:core, ... to 3.9.3 (#5408)
7ea72f279f Update logback-classic to 1.5.0 (#5406)
3f22e42ad4 Update scalatest to 3.2.18 (#5395)
5544a2efc4 Update janino to 3.1.12 (#5387)
e30d1b1cad Update slf4j-api to 2.0.12 (#5388)
b45b8b7eb0 Update sbt-bloop to 1.5.15 (#5386)
843e9ee23a Update sqlite-jdbc to 3.45.1.0 (#5384)
5bd5cc51dc Update metrics-core, metrics-healthchecks, ... to 4.2.25 (#5372)
62012b1ee6 Upgrade microPickle to 3.1.4 (#5368)
217914432e Update scalapb-runtime to 0.11.15 (#5367)
f6648a1cc3 Update metrics-core, metrics-healthchecks, ... to 4.2.24 (#5360)
3c6fd85f4d Update sqlite-jdbc to 3.45.0.0 (#5361)
e6ceda44f2 Update play-json to 2.10.4 (#5356)
1ad2778ab2 Update client3:core, ... to 3.9.2 (#5355)
510ac32db5 Update metrics-core, metrics-healthchecks, ... to 4.2.23 (#5320)
495197d380 Update postgresql to 42.7.1 (#5321)
001a2f0970 Update slf4j-api to 2.0.11 (#5351)
589fc0c98b Update sbt-scalajs, scalajs-compiler, ... to 1.15.0 (#5348)
e8052e5a91 Update sbt-assembly to 2.1.5 (#5305)
63b1f05b2e Update sbt-bloop to 1.5.13 (#5319)
329241b332 Update sbt-mdoc to 2.5.2 (#5350)
37eaabab22 Update jna to 5.14.0 (#5347)
7a3834dc80 Update sbt to 1.9.8 (#5349)
ecbd024465 Update janino to 3.1.11 (#5313)
39b127be8c Update sbt-bloop to 1.5.12 (#5309)
bb549b46d4 Update logback-classic to 1.4.14 (#5314)
23cc9c9acc Update junixsocket-core to 2.8.3 (#5300)
36d27704b4 Update bcprov-jdk18on to 1.77 (#5302)
a21b0a1741 Update client3:core, ... to 3.9.1 (#5296)
222ad3b60d Update sbt-mdoc to 2.5.1 (#5298)
7861bce71a Update native-lib-loader to 2.5.0 (#5297)
c1baad9f06 Update sbt-mdoc to 2.5.0 (#5289)
665727f165 Update play-json to 2.10.3 (#5282)
33087f6ec1 Update metrics-core, metrics-healthchecks, ... to 4.2.22 (#5283)
5273af05d8 Upgrade website dependencies (#5279)
336cb60c51 Update sbt-assembly to 2.1.4 (#5275)
a513cab030 Update sbt-bloop to 1.5.11 (#5226)
763da5619c Update junixsocket-core to 2.8.2 (#5276)
11f07a1cd1 Update sbt to 1.9.7 (#5271)
984a2344bc Update scodec-bits to 1.1.38 (#5273)
a47e18187d Update play-json to 2.10.2 (#5270)
087508dadc Update sbt-mdoc to 2.4.0 (#5266)
b085045e16 Update metrics-core, metrics-healthchecks, ... to 4.2.21 (#5261)
d256f3cdb1 Update scalapb-runtime to 0.11.14 (#5264)
723640b8f6 Update typesafe:config to 1.4.3 (#5265)
d65bd11bd6 Update sbt-mdoc to 2.3.8 (#5257)
8752d9fe70 Update junixsocket-core to 2.8.1 (#5252)
7c133c19ca Update jeromq to 0.5.4 (#5246)
046a9e869c Update sbt-scalajs, scalajs-compiler, ... to 1.14.0 (#5245)
08cb329719 Update metrics-core, metrics-healthchecks, ... to 4.2.20 (#5244)
922a6aaeb4 Update play-json to 2.10.1 (#5239)
00bd36ad2e Update sbt-assembly to 2.1.3 (#5238)
6373b5b866 Update junixsocket-core to 2.7.2 (#5236)
1edf57ac1e Update sbt to 1.9.6 (#5237)
d7037ede28 Update scala-library to 2.13.12 (#5235)
a0b273e6b3 Update junixsocket-core to 2.7.1 (#5234)
b474a60986 Update sbt-scoverage to 2.0.9 (#5233)
4146f856ed Update scalatest to 3.2.17 (#5232)
433a097449 Update slf4j-api to 2.0.9 (#5228)
d37faa0bcf Update sqlite-jdbc to 3.42.0.1 (#5205)
ccafd0c5c9 Update sbt to 1.9.4 (#5204)
2fe80bd52b Update scala-collection-compat to 2.11.0 (#5107)
561be207b1 Update sbt-coveralls to 1.3.11 (#5191)
6c9d71cb97 Update logback-classic to 1.4.11 (#5189)
38f8c40c33 Update client3:core to 3.9.0 (#5190)
d5bf14f5cf Update sbt-coveralls to 1.3.10 (#5184)
b1ca4310ac Update logback-classic to 1.4.9 (#5178)
969937b9df Update junixsocket-core to 2.7.0 (#5179)
eff3580ea0 Update bcprov-jdk18on to 1.76 (#5175)
092ef944e2 Update sbt to 1.9.3 (#5160)
516bfdd007 Bump semver from 5.7.1 to 5.7.2 in /website (#5149)
4622080b5b Bump json5 from 2.2.1 to 2.2.3 in /website (#4943)
115da94229 Update sbt to 1.9.2 (#5142)
cde3977312 Update sbt-scalajs-crossproject to 1.3.2 (#5137)
51f5f53322 Update client3:core to 3.8.16 (#5134)
cd812ac7a3 Update janino to 3.1.10 (#5128)
aff86c7679 Update sbt to 1.9.1 (#5123)
2023e3cfb7 Update bcprov-jdk18on to 1.75 (#5112)
3df12aa929 Update sbt-scalajs, scalajs-compiler, ... to 1.13.2 (#5113)
2a966a6916 Update bcprov-jdk18on to 1.74 (#5106)
2363ec4d4e Update logback-classic to 1.4.8 (#5103)
ebe98959ed Upgrade to scalac `2.13.11` (#5097)
514cb54f69 Update metrics-core, metrics-healthchecks, ... to 4.2.19 (#5090)
3166539d2a Update sbt to 1.9.0 (#5091)
7036915e0f Update sbt-scoverage to 2.0.8 (#5092)
46b9301c5d Update sqlite-jdbc to 3.42.0.0 (#5083)
6f486bcc68 Fix transitive dep introduced in sqlite3 3.41.2.2 (#5082)
f99113f2f8 Update sqlite-jdbc to 3.41.2.2 (#5080)
a60f62b59d Update scalacheck-1-16 to 3.2.14.0 (#5079)
200d15af21 Update scalatest to 3.2.16 (#5078)
17ee387e40 Update sbt to 1.8.3 (#5077)
1014c47bd9 Update bcprov-jdk18on to 1.73 (#5044)
8aadf71eb1 Update sbt-native-image to 0.3.4 (#5067)
09150e5a1f Update scalacheck-1-15 to 3.2.12.0 (#5068)
475ed33716 Update sbt-ci-release to 1.5.12 (#5064)
a7146e7dce Update scala-collection-compat to 2.10.0 (#5061)
9f129c2fe5 Update logback-classic to 1.4.7 (#5054)
760903660e Update sbt-scalajs-crossproject to 1.3.1 (#5047)
25cdf9d880 Update client3:core to 3.8.15 (#5042)
3379e99ffd Update sbt-scalajs, scalajs-compiler, ... to 1.13.1 (#5043)
5e668c5d5e Update client3:core to 3.8.14 (#5028)
d2cc7ec98b Update sbt-scalajs-crossproject to 1.3.0 (#5035)
8186b2e2df Update waffle-jna to 3.3.0 (#5026)
b2ead6038f Update sqlite-jdbc to 3.41.2.1 (#5027)
d3d85eae95 Update sqlite-jdbc to 3.41.0.1 (#5019)
51a354245b Update client3:core to 3.8.13 (#5010)
84803fdfb1 Update metrics-core, metrics-healthchecks, ... to 4.2.18 (#5013)
efd92c1280 Update logback-classic to 1.4.6 (#5012)
64354d2eb9 Update postgresql to 42.6.0 (#5014)
f221688fe0 Update slf4j-api to 2.0.7 (#5015)
fbcea17e2f Update client3:core to 3.8.12 (#5007)
6aae816de9 Update metrics-core, metrics-healthchecks, ... to 4.2.17 (#4999)
b444a192b0 Update sbt-native-packager to 1.9.16 (#4998)
cea5dee58b Update scodec-bits to 1.1.37 (#5006)
ccb3b27fb3 Update sqlite-jdbc to 3.41.0.0 (#5001)
4b5659d740 Update scodec-bits to 1.1.36 (#5004)
c192ede1d8 Update postgresql to 42.5.4 (#4993)
a9e17684fe Update sbt-native-packager to 1.9.15 (#4989)
46dc3ed14e Update metrics-core, metrics-healthchecks, ... to 4.2.16 (#4990)
80d0bd2560 Update sbt-scoverage to 2.0.7 (#4982)
46d3079e5b Update scodec-bits to 1.1.35 (#4979)
6903db941d Update org.osgi.service.jdbc to 1.1.0 (#4929)
567f75df18 Update jna to 5.13.0 (#4951)
90549a68ab Update sbt-scalajs, scalajs-compiler, ... to 1.13.0 (#4963)
473664f932 Update junixsocket-core to 2.6.2 (#4981)
7043c95800 Update play-json to 2.9.4 (#4959)
853bd2387e Update sbt-mdoc to 2.3.7 (#4964)
0ff1370bf9 Update postgresql to 42.5.3 (#4972)
2445e4fa8b Update sqlite-jdbc to 3.40.1.0 (#4969)
0848cb6478 Update client3:core to 3.8.11 (#4977)
79476a6231 Update scalapb-runtime to 0.11.13 (#4978)
7d749f271b Update sbt-native-packager to 1.9.14 (#4980)
a544ce0287 Update sbt-native-packager to 1.9.13 (#4953)
4b4f6c8b9f Update sbt-native-packager to 1.9.12 (#4952)
bfb7469209 Update client3:core to 3.8.8 (#4948)
06a8e15e79 Update gson to 2.10.1 (#4945)
642fad6fe4 Update client3:core to 3.8.7 (#4946)
c729c906a1 Update scalatest to 3.2.15 (#4947)
7ed62ceeba Upgrade sbt to 1.8.2 (#4940)
7e8002f626 Update client3:core to 3.8.6 (#4938)
48a2a8f5e6 Update metrics-core, metrics-healthchecks, ... to 4.2.15 (#4930)
ba65426ca5 Update sbt-bloop to 1.5.6 (#4915)
2ae87278cd Update slf4j-api to 2.0.6 (#4922)
424ea33541 Update jeromq to 0.5.3 (#4912)
3bc9d8808a Update client3:core to 3.8.5 (#4909)
84ce9b6f56 Update scala-collection-compat to 2.9.0 (#4910)
4f9cfdbbba upgrade website deps (#4907)
ea5a66a80e Update scala-java-time to 2.5.0 (#4903)
6fae0abaee Update postgresql to 42.5.1 (#4898)
6efa60083a Update slf4j-api to 2.0.5 (#4901)
aa54f28a60 Update sbt-scalajs, scalajs-compiler, ... to 1.12.0 (#4899)
6a0df4ee14 Update sqlite-jdbc to 3.40.0.0 (#4895)
1d4ee6cba4 Update logback-classic to 1.4.5 (#4892)
caaee2976c Update slf4j-api to 2.0.4 (#4890)
dac72f53d8 Update metrics-core, metrics-healthchecks, ... to 4.2.13 (#4893)
5061fc6fcc Update sqlite-jdbc to 3.39.4.1 (#4891)
775395874e Update janino to 3.1.9 (#4888)
dbd54ca55f 2022 11 11 sbt 1.8.0 (#4883)
4df71ea9d8 Update sbt-scalajs-bundler to 0.21.1 (#4887)
1127c56a98 Bump versions to 1.9.7 (#4886)