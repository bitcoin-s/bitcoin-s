# 1.9.9

This release is backwards compatible with the 1.9.x series of bitcoin-s

See the individual module sections for more information on lower level updates to the codebase.

Want to get started quickly? See our `docker-compose.yml` file. [See instructions here](https://github.com/bitcoin-s/bitcoin-s/#docker)

If you are a typescript developer, [you can access the backend via our typescript library](https://github.com/bitcoin-s/bitcoin-s-ts)

# Executive Summary

This release updates our RPC support, adds descriptors to `core` and updated our build to work towards support of scala3.

## Running bitcoin-s

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-1.9.9.zip` and then run it with `chmod +x ./bin/bitcoin-s-server && ./bin/bitcoin-s-server` to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-1.9.9.zip` folder and start using the `bitcoin-s-cli` like this:

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

3d29bef135a Delete directory using Files.delete() (#5518)

## App server

## bitcoind rpc

This release adds support for `25.x`, `26.x`, and `27.x` of bitcoind's RPC interface.

This release removes support for `0.21`, `22.x`, `23.x`, and `24.x` of bitcoind's RPC interface.

18cb4f3ddf8 Add -datadir to when obtaining bitcoind -version to avoid using default datadir (#5574)
c8266cfb511 Implement `getrawmempool` verbose (#5573)
5d0056a01f5 Add missing fields to `GetMemPoolInfoResult` (#5572)
cb3fbe523a1 Rework BitcoindRpcTestUtil.getBinary() to match major and minor versions of bitcoind binary when possible (#5569)
6543b261c41 Implement `bitcoind` RPC 27.0 (#5552)
7ef60866733 Implement bitcoind RPC `26.x` (#5550)
5dc5cca9cff 2024 04 24 rm v24 (#5549)
d23d7851b85 rework `bitcoind-rpc` to take a `walletName` parameter that is a String (#5545)
1d42de09771 Make `wallet.dat` be the default wallet name for bitcoind in our codebase (#5543)
23e32652f3c Implement bitcoind `25.2` (#5540)
68d267ab8c7 2024 04 21 bitcoindrpc testfixtures (#5539)
f7adb6264e5 Remove support for 23.x of bitcoind (#5538)
7b3463229fd 2024 04 21 Remove `BitcoindV22RpcClient` (#5537)
9442dba217a Remove support for `v21` of `bitcoind` (#5496)
c01793dc890 Implement importdescriptors RPC (#5534)
e143792fb96 2024 04 16 bitcoindrpc descriptor (#5530)
d162242a39b 2024 03 24 v21 rpc refactor (#5494)

## Build

This release adds the `-Xsource:3` compiler flag to all modules except [`lnd-rpc`](https://github.com/bitcoin-s/bitcoin-s/issues/5591).

This release also bumps the java version in our docker files from `17` -> `21`.

2b1f85e55c8 2024 05 14 mac electron release (#5594)
a6b7fada504 Get rest of codebase compiling with -Xsource:3 (#5592)
05894e8c548 2024 05 10 keymanager xsource3 (#5583)
37fc215b554 2024 05 10 `appCommons/` compiling with `-Xsource:3` (#5582)
d13e12afaee 2024 05 11 wallet xsource3 (#5588)
0af58483f2d Add -Xsource:3 to rpc projects (#5590)
562e5602e33 Add -Xsource:3 to node/ node-test/ tor/ tor-test/ (#5589)
ac7739444fd Add -Xsource:3 to `chain/` (#5587)
791ff3282a3 Add -Xsource:3 to db-commons/ (#5585)
4b17645c1bf Add -Xsource:3 to docs/ (#5581)
654d4086b99 2024 05 08 scala3 `core` (#5580)
f2ae03bc528 Fix jlinkIgnore after bumping waffle-jna to 3.4.0 (#5577)
9fe67bbdbe1 Remove remaining 2.13 specific code as 2.13.x is the only compiler version supported (#5576)
3af204e74da Pull over scala3 compatible code for `crypto/` project  (#5575)
f707db0a0e6 Try to fix release build to workaround  (#5579)
90e370c0ef1 Remove TorTests.yml as it spuriously fails on CI connecting to the tor network, add build commands to Compile.yml (#5578)
4f288a12c0a 2024 05 02 intellij compile (#5562)
73279753d7a Renable `parallelExecution` for `bitcoindRpcTest` (#5541)
2102e4d6828 Try `brew install sbt` as workaround for macOS Ci runners failing to install sbt (#5555)
97fe7956931 Update dialect to scala213source3 (#5536)
dc16ddfc2de Migrate from `setup-scala` github action to `setup-java` github action in `release.yml` (#5504)
723e2f6a319 Bump java-version from 17 -> 21 on CI (#5503)

## chain

376872eb3e4 Remove different versions of Blockchain.scala now that we don't support scalac 2.12 (#5519)

## cli

## clightning rpc

4f791d71d7e Upgrade clightning to `24.02.2` (#5558)

## Core

This release adds support for [descriptors](https://github.com/bitcoin/bips/blob/master/bip-0380.mediawiki).
This release does not integrate the descriptors into our wallet module, yet.

a6d93622f89 2024 04 16 descriptor fidelity (#5529)
0fa3be37ddf 2024 04 08 Descriptors (#5525)
2536fd31386 `TapscriptTree`, `TapBranch`, `TapLeaf` (#5520)
790327639a8 Tighten P2WSHWitnessSPKV0.apply() to only take RawScriptPubKey (#5509)
4ae9067083c Add invariant to P2WPKHWitnessSPKV0.apply() to make sure ECPublicKey is compressed now that we have #5502 (#5508)
c2cbaae88f6 Add invariant to make sure we can't have p2sh(p2sh()) (#5507)
99c1292ddc9 Add `xor` operator as `NumberType.{^,xor()}` (#5500)

## Crypto

This release changes the behavior of `ECPublicKey.bytes`. Previously we would _always_ return the compressed version
of the public key, even if we created the `ECPublicKey` instance with a non-compressed byte representation.
Now `ECPublicKey.bytes` will return the byte representation that was passed as a parameter.

This release also repurposes the `PublicKey` trait. Now it is extended by all public key types in the codebase
rather than just `ECPublicKey` types. This was needed for descriptor support in `core`.

b83661e73f7 Repurpose PublicKey trait, extend it with {SchnorrPublicKey, ECPublicKeyApi, XOnlyPubKey} (#5517)
41c835761ca Make `ECPublicKey` return `bytes` that were passed as a parameter  (#5502)

## db commons

## DLC node

## DLC Oracle

## DLC Server

## DLC wallet

## Eclair rpc

863ffd9d6fc Adjust `OutgoingPaymentStatus.completedAt` to be `RelayTimestamp` type (#5586)
7ed2b8801ab 2024 04 30 upgrade eclair `v0.10.0` (#5557)
7ee749adcb8 2024 04 29 eclair upgrade `v0.9.0` (#5556)

## gui

## fee rate

c2b8ae98eeb Re-enable mempool api tests (#5560)
49153505940 Ignore mempool testnet api tests for now as unit tests consistently fail (#5547)

## keymanager

## Lnd rpc

819a047d1e3 Add support for lnd `0.17.5-beta` (#5554)

## Lnurl

## node

This release fixes a bug where we could deadlock our node's stream when attempting to send a message to our peer.

36ec8a29118 Rework PeerManagerApi.{sendToRandomPeer, gossipMessage} to return Unit rather than Future[Unit], this removes the possibility of deadlocking on a full queue (#5561)
302110cb56c Bump max connected peers default to 2 (#5515)

## Oracle Explorer Client

## wallet

## testkit-core

## testkit

## tor

## Website

## ZMQ

## Dependencies

8288b2ae1e3 Update scodec-bits to 1.2.0 (#5593)
f300bed25ab Update waffle-jna to 3.4.0 (#5571)
6dacfb071a6 Upgrade to sbt 1.10.0 (#5570)
1dcc30734db Update play-json to 3.0.3 (#5566)
16229889abe Update scala-library to 2.13.14 (#5567)
e70f2ae11a8 Update client3:core, ... to 3.9.6 (#5565)
d6880b328d6 Update sbt-scoverage to 2.0.12 (#5568)
368e0ac3d51 Remove scala-async dep as it is not used (#5535)
899d4916835 Update logback-classic to 1.5.6 (#5531)
afddf73c48d Update scalafmt-core to 3.8.1 (#5501)
2b497c634f5 Update scalacheck to 1.18.0 (#5533)
fd4c4d3f8a6 Update bcprov-jdk18on to 1.78.1 (#5532)
6f6a78ab527 Update scalacheck to 1.17.1 (#5527)
b83e577d41d Update sqlite-jdbc to 3.45.3.0 (#5528)
d39d89bfede Update scala-collection-compat to 2.12.0 (#5526)
c328ec860ee Update sbt-native-packager to 1.10.0 (#5522)
c26bd8db9b8 Update slick, slick-hikaricp to 3.5.1 (#5524)
24a0bd43f88 Update logback-classic to 1.5.5 (#5521)
81a72b20fe9 Update slf4j-api to 2.0.13 (#5523)
f20ea7423c4 Update logback-classic to 1.5.4 (#5516)
d3fca354a4b Update otj-pg-embedded to 1.0.3 (#5512)
4377b082e37 Update scalamock to 6.0.0 (#5514)
4bb1724080e Update sbt-bloop to 1.5.17 (#5510)
790708daa64 Update junixsocket-core to 2.9.1 (#5511)
9b93a5a1ef2 Update bcprov-jdk18on to 1.78 (#5513)
c747f1482a3 Update play-json to 3.0.2 (#5359)
dae7b0517b3 Update sbt-bloop to 1.5.16 (#5499)
2cb358fa4c9 Run yarn upgrade to update website deps (#5495)
