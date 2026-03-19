# 1.9.12

This release is backwards compatible with the 1.9.x series of bitcoin-s

See the individual module sections for more information on lower level updates to the codebase.

Want to get started quickly? See our `docker-compose.yml`
file. [See instructions here](https://github.com/bitcoin-s/bitcoin-s/#docker)

If you are a typescript
developer, [you can access the backend via our typescript library](https://github.com/bitcoin-s/bitcoin-s-ts)

# Executive Summary

This release

- Publishes native arm64 binaries for macOS in addition to linux
- Updates native libsecp256k1 binaries
- Adds support for pure JVM FROST (Threshold Signatures)
- Updates the pure JVM MuSig2 implementation to be BIP327 compliant
- Adds support for bitcoind v30
- Improves performance of database connection handling and node message processing

## Running bitcoin-s

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-1.9.12.zip` and then run it with `chmod +x ./bin/bitcoin-s-server && ./bin/bitcoin-s-server`
to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-1.9.12.zip` folder and start using the `bitcoin-s-cli` like this:

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

https://central.sonatype.com/repository/maven-snapshots/

# Modules

## app commons

## App server

d8baef0068a Add Taproot witness support to decodeRawTransaction (#6194)

## bitcoind rpc

This release adds support for bitcoind v30.2 and removes support for v27.

ca8d37e868a bitcoindRpc: Add support for bitcoind-rpc v30.2 (#6177)
dbdd1a21e4d bitcoindRpc: Remove support for bitcoind v27 now that it is EOL (#6115)
69debb672f5 Bump to bitcoind v29.2 (#6114)
392cc7c491d 2025 10 01 bitcoind v30 (#6095)
57960f22b34 bitcoindRpc: Cleanup old bitcoind data strutures (#6122)

## Build

1bba4a3a528 Pull over modules from #5713 that have source compatible scala3 changes (#6257)
765a6207a68 Remove -Xms512m, bump -Xmx to 8gb from 4gb (#6229)
d58ca4eda77 Add native ARM64 Mac jlink support using GitHub macos-15 runners (#6201)
85921242310 ci: Use zulu jdk for docker release to avoid temurin jlink/jdep bug (#6104)
1aaa397213c ci: Update CI runners to java 25 (#6101)

## chain

This release includes optimizations for block header ancestors and chain verification.
This implements consensus checks for timestamps in block headers.

d092aed967a chain: Make `CompactFilterDAO` queries lazily created to avoid hotspots in `PeerManager` (#6218)
279c187f307 chain: Optimize getBlockchainsBetweenHeights() (#6214)
dde9255337e chain: Optimize BlockHeaderDAO.getAncestorAtHeight() (#6211)
dab5bbe3d3a Move 'getmediantimepast' to Blockchain data structure (#6164)
1a91847f617 chain: Fix ChainApi.nextFilterHeaderBatchRange() when we request build a FilterSyncMarker for compact filter
headers we have already seen (#6080)

## cli

## clightning rpc

a73590fd0ea clightningRpc: Upgrade clightning RPC to 25.09 (#6108)
d487a64f133 Bump clightning to v25.02 (#6106)
452970670ea clighntingRpc: Update clightning to 24.11.2 (#5896)

## Core

This release adds support for FROST and updates MuSig2 to be BIP327 compliant.
It also includes significant performance improvements for script deserialization.

0c311703faa core: Cache the byteVector representation of our Script in `ScriptProgram` (#6232)
00de15152e7 core: Add ability to construct a scriptsig from the scriptPubKey it is spending (#6152)
f7fe6979f32 core: Cleanup NetworkPayload.scala (#6118)
3ce7b6890a1 core: Move NodeConstants to core, use it in the version message (#6116)
f568589be39 core: Fix CompactFilterHeaderDb.toString() (#6083)
ca9e8d87cb6 Fix Taproot fundingOutputs ordering to comply with BIP-341 (#6129)
a0ec5e5ccf3 core: Make NotFoundMessage not inherit from InventoryMessage, handle NotFoundMessage differently than
InventoryMessage in node (#6175)
58533ffcab7 2026 01 16 update libsecp256k1-zkp (#6181)

## Crypto

This release adds support for FROST (Threshold Signatures) and updates MuSig2 to be BIP327 compliant.

d70d242baaa Adapt MuSig2 API to adhere to BIP327
2344c18252f 2026 01 13 FROST signing and verification (#6190)
1ca8b3bb612 crypto: Add XOnlyPubKey.verify() helpers to verify schnorr signatures with a xonly public key (#6230)

## db commons

This release re-enables teh hikari connection pool by default. The pool size is 4 connections.
This release also modifies the parameters given to sqlite connections to improve performance and reliability.
A new configuration available is `bitcoin-s.{module-name}.dbCommons.busyTimeout` which sets the `busy_timeout` parameter
for sqlite connections,
which can help avoid `SQLITE_BUSY` exceptions when multiple connections are trying to access the same database file.
By default this is set to 30 seconds.

99750b0af13 2026 03 11 Bump database connection pool size from `1` -> `4` by default (#6253)
ec7eab327ab dbCommons: sqlite: Change `transaction_mode` to `IMMEDIATE` for sqlite (#6252)
49bc2893014 dbCommons: Rework DbAppConfig to allow for multiple database connection pools over the lifetime of the
DbAppConfig class on the JVM (#6245)
4276f547a1d dbCommons: config: Add BUSY_TIMEOUT configuration setting for sqlite databases (#6250)
41f5244feed dbCommons: Fix setting WAL for sqlite connections, don't register mbeans (#6246)

## DLC node

## DLC Oracle

## DLC Server

## DLC wallet

cb2173c0488 Remove old DLC migration code (#6248)
9bacdb7d69c Fix missing default parameter in DLCAppConfig (#6244)

## Eclair rpc

18e50540bf2 eclairRpc: Update Eclair RPC interface to v13 (#6107)
5f869f6f8d4 2025 10 07 eclair v12 (#6105)

## gui

## fee rate

## keymanager

## Lnd rpc

a68b7a3b973 Upgrade lnd RPC interface to support v0.19.3-beta (#6109)

## node

This release improves node message processing and peer management.

74b063bfd5a node: Add receivedAt timestamp for ControlMessages (#6213)
bc30c9d2db6 node: Bump # of parallel offers to PeerManager from 1 -> FutureUtil.getParallelism (#6206)
a09e719b9fd node: Add receivedAt field to DataMessageWrapper, add more logs around the amount of time to process a
DataMessage (#6204)
4f461caa343 node: Remove the duplicate chainConfig/nodeConfig in NeutrinoNode (#6127)
8135505f7b6 node: Fix bug where we weren't processing the block if we hadn't seen its header before (#6126)
728c9429470 node: Store VersionMessage in PeerData (#6121)
c50f6562d3f node: Add logs to indicate what peer a message originated from (#6096)
a0b48b00161 node: Add invalid block header error message to log (#6088)
448ce39d511 node: Fix bug when removing a peer from NodeState when we are in the MisbehavingPeer state (#6076)
3e7b76762a0 node: Add invariant to NodeState that says you cannot both be a steady state peer and be waiting for
disconnection (#6075)

## wallet

The release improves the performance of transaction processing and adds database indexes to `txo_spending_info` for
`txid` and `txo_state`.

The processing of a single transaction gossiped on the network is improved by 100x in some cases.

This release also fixes a bug where we weren't properly subscribing to the
block processing completion signal in `TransactionProcessing`,
which could lead to receiving too many blocks too fast
or a malicious peer never sending a block without us noticing.

5330f59c6b1 wallet: Cache keymanager (#6242)
e29ae91e4a3 wallet: Add database indexes to `txo_spending_info` for `txid`,`txo_state` (#6215)
98d9c623825 wallet: Change `processTransaction()` return type from `Unit` -> `ProcessTxResult` (#6205)
2565a4524cf wallet: Fix bug in `TransactionProcessing.subscribeForBlockProcessingCompletionSignal()` (#6136)

## scripts

## testkit-core

## testkit

This release modernizes the test infrastructure to use testcontainers and improves test reliability.

38e0516135f testkit: Renable hikari connection pool by default in test cases (#6251)
2b1917be401 2026 03 09 `AppConfig` lifecycle management in test fixtures (#6249)
fa04f67e1af 2026 02 12 Rework tests to use testcontainers directly (#6221)
ad7b8604508 testkit: Modernize `chainTest` test fixtures, use them in `chainTest` (#6157)
eb9bda3cb32 testkit: Set eclair.router.channel-spent-splice-delay to 6 (#6237)
b6cb43c6e0f tests: Fix bug where we wouldn't generate enough property based tests in `cryptoTest` (#6224)
9f7d3bba03e tests: Simplify TaprootTxTests to give better performance on CI (#6212)

## tor

## Website / Deps

9b6c653f2b4 Add frost.md documentation for FROST threshold signature scheme (#6239)
04bb2ab9f0e Add Copilot instructions for repository (#6196)

e3a3045f6b3 Update sqlite-jdbc to 3.51.3.0 (#6255)
8995f7a09f0 Update sbt, scripted-plugin to 1.12.6 (#6254)
56febe136a2 Update xz to 1.12 (#6241)
b57a8d02462 Update sbt, scripted-plugin to 1.12.5 (#6240)
ae591d8b574 Update typesafe:config to 1.4.6 (#6234)
16089096ee3 Update sbt, scripted-plugin to 1.12.4 (#6235)
59317dc3614 Update waffle-jna to 3.6.0 (#6228)
c2b7e2b0ab2 deps: Update bcrypto to latest release 5.5.2 (#6233)
49d09667395 Update sbt, scripted-plugin to 1.12.3 (#6226)
afb17b53e07 Update logback-classic to 1.5.32 (#6225)
dad63788095 Update logback-classic to 1.5.30 (#6222)
a5bc9457bf2 Update postgresql to 42.7.10 (#6219)
d10d257bea9 Update sqlite-jdbc to 3.51.2.0 (#6220)
23f25f106d2 Update sqlite-jdbc to 3.51.1.1 (#6217)
d1bb9e6d608 Update logback-classic to 1.5.29 (#6216)
f6cd7210ed9 Update logback-classic to 1.5.28 (#6203)
1e39053c92c Update sbt-bloop to 2.0.19 (#6197)
9952afc8e7b Update sbt, scripted-plugin to 1.12.2 (#6198)
444f390e98f Update scalamock to 7.5.5 (#6191)
ca368761e9c Update logback-classic to 1.5.27 (#6189)
e62d38dbdec Update sbt, scripted-plugin to 1.12.1 (#6187)
81c028bb39b Update scalamock to 7.5.4 (#6188)
63c69a1b8c3 Update metrics-core to 5.0.6 (#6186)
7875cf1836a Update metrics-core, metrics-healthchecks, ... to 4.2.38 (#6185)
32752e0b341 Update logback-classic to 1.5.26 (#6184)
c009967bc4f Update logback-classic to 1.5.25 (#6182)
47389cb2e5d Update postgresql to 42.7.9 (#6183)
6499316c65e Update sbt-bloop to 2.0.18 (#6179)
a1ad645f209 Update sbt-scoverage to 2.4.4 (#6180)
3b579d42fb3 Update sbt-native-packager to 1.11.7 (#6178)
5cc1be16ce1 Update scalamock to 7.5.3 (#6176)
e752d234cae Update logback-classic to 1.5.24 (#6174)
7baccc501fa Update sbt-unidoc to 0.6.1 (#6167)
1de7d9d04e4 Update sbt-scalajs, scalajs-compiler, ... to 1.20.2 (#6168)
0fedec52878 Update sbt, scripted-plugin to 1.12.0 (#6169)
52b59124dc1 Update sbt-mdoc to 2.8.2 (#6166)
160e99c4c04 Update logback-classic to 1.5.23 (#6165)
c5d919a9761 Update os versions from static 'macos-13' and 'ubuntu-22.04' to 'macos-latest' and 'ubuntu-latest' (#6163)
c9979de4c81 Update logback-classic to 1.5.22 (#6159)
0f69ee071e4 Update pekko-actor, pekko-discovery, ... to 1.4.0 (#6160)
ca1fd2acdc3 Update sbt-scoverage to 2.4.3 (#6161)
549fdac4d99 Update sbt-mdoc to 2.8.1 (#6156)
c7cee546a24 Update sqlite-jdbc to 3.51.1.0 (#6153)
c9aa414492f Update bcprov-jdk18on to 1.83 (#6148)
811cd0df997 Update scala-library to 2.13.18 (#6149)
5a4787b0af1 Update pekko-actor, pekko-discovery, ... to 1.3.0 (#6145)
5ff71c5e811 Update xz to 1.11 (#6146)
b6c1007cf7f Update sbt-scoverage to 2.4.2 (#6142)
5dfeab7ca68 Update sbt-bloop to 2.0.17 (#6141)
53763a0322a Update logback-classic to 1.5.21 (#6140)
f5829661958 Update sbt-scoverage to 2.4.1 (#6138)
48eb546f397 Update scalamock to 7.5.2 (#6137)
301ba5c22ad Update sqlite-jdbc to 3.51.0.0 (#6134)
494fea0a899 Update scalamock to 7.5.1 (#6132)
5ea2db56c00 Update sbt-scalafmt to 2.5.6 (#6130)
e97ba13b22c Update pekko-http, pekko-http-testkit to 1.3.0 (#6125)
494ff0a154d Update pekko-grpc-runtime, ... to 1.2.0 (#6124)
93924ffe880 Update sbt-mdoc to 2.8.0 (#6111)
bd12152dbe3 Update scala-collection-compat to 2.14.0 (#6103)
d3150e81e7b Update scala-library to 2.13.17 (#6102)
30b6aab619f Update logback-classic to 1.5.20 (#6119)
d15b9bc25ec Update sbt-scoverage to 2.4.0 (#6120)
60bf3f6e537 Update play-json to 3.0.6 (#6110)
35db031db35 Update sbt-bloop to 2.0.16 (#6094)
932bb86fb12 Update sbt, scripted-plugin to 1.11.7 (#6098)
339c8150297 Update logback-classic to 1.5.19 (#6090)
c2e434f3c6a Update sbt-native-packager to 1.11.4 (#6091)
6df5df8e4ef Update jna to 5.18.1 (#6092)
53151adf15a Update metrics-core to 5.0.5 (#6078)
a65b1e5f52a Update pekko-actor, pekko-discovery, ... to 1.2.1 (#6086)
06964092fc4 Update jna to 5.18.0 (#6085)
42681a3e332 Update sbt-bloop to 2.0.14 (#6084)
12ae1792729 Update guava to 33.5.0-jre (#6081)
7010429203c Update bcprov-jdk18on to 1.82 (#6079)
6743f70385b Update postgresql to 42.7.8 (#6082)
dfcad298f2d Update metrics-core, metrics-healthchecks, ... to 4.2.37 (#6077)
3712d54f22b Update scalamock to 7.5.0 (#6071)
1f29801bef5 Update typesafe:config to 1.4.5 (#6070)
3f35a467bbb Update gson to 2.13.2 (#6069)
8e95b2128fc Update sbt-scalafmt to 2.5.5 (#6020)
2092f53125b Update scalacheck to 1.19.0 (#6062)
2f5f6f9ada6 Update scalamock to 7.4.2 (#6063)
ca0ea6c1e15 Update sbt-scalajs, scalajs-compiler, ... to 1.20.1 (#6060)
363a623932c Update sbt, scripted-plugin to 1.11.6 (#6061)

