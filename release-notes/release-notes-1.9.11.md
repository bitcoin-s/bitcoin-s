# 1.9.11

This release is backwards compatible with the 1.9.x series of bitcoin-s

See the individual module sections for more information on lower level updates to the codebase.

Want to get started quickly? See our `docker-compose.yml` file. [See instructions here](https://github.com/bitcoin-s/bitcoin-s/#docker)

If you are a typescript developer, [you can access the backend via our typescript library](https://github.com/bitcoin-s/bitcoin-s-ts)

# Executive Summary

This release

- Improves performance of serialization/deserialization in `core`
- Adds support for the `testnet4` network. You can set this with `bitcoin-s.network = "testnet4"` in your `bitcoin-s.conf`
- Adds support for `v29` of bitcoind
- Installs the datadir in the correct place for mac installs


## Running bitcoin-s

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-1.9.11.zip` and then run it with `chmod +x ./bin/bitcoin-s-server && ./bin/bitcoin-s-server` to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-1.9.11.zip` folder and start using the `bitcoin-s-cli` like this:

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

Starting with Bitcoin-S 1.9.11, the macOS data directory has moved:
- Before 1.9.11: ~/.bitcoin-s 
- 1.9.11 and later: ~/Library/Application Support/bitcoin-s

This change aligns with Apple’s guidelines for application data storage. macOS expects apps to keep user-specific support files in the Application Support folder inside your Library directory. This keeps your home folder organized, ensures compatibility with macOS backups (like Time Machine), and follows best practices for sandboxed and secure applications.

Your existing data will not be automatically migrated. If you want to keep your old data, you can manually move the contents from ~/.bitcoin-s to ~/Library/Application Support/bitcoin-s.

fd3901fa285 Add migration code to put app data in proper spot on mac for fresh installs (#5622)
4f1be339308 appServer: Fix bug where we weren't implementing bitcoind callbacks correctly for rescans (#5998)

## bitcoind rpc

This release adds support for v29 of bitcoind and bumps the minor releases to 28.2 and 27.2.

2a8fbc44470 bitcoindRpc: Fix `listUnspent()` for `Vector[BitcoinAddress]` param (#6038)
1b139d825ab Add support for bitcoind 28.2 (#6037)
82380378cf7 2025 07 29 bitcoind rpc v29 (#6033)
f97d61c0d78 bitcoindRpc: Remove support for v26 of bitcoind-rpc as its past EOL (#6032)
7b1fd8e7592 bitcoindRpc: Implement 'scantxoutset' RPC (#5936)
9d84ff5d035 bitcoindRpc: Make 'gettxout' RPC handle null return correctly (#5930)
7eef13efa0f Update bitcoind to 28.1/27.2 (#5893)

## Build

We now publish our SNAPSHOT release to central sonatype. For more information please read [here](https://central.sonatype.org/publish/publish-portal-snapshots/)

ab8d2944d0f build: Remove sonatype build settings as it doesn't fix redirect loop (#6014)
7bc7a773139 Explicitly state the publishTo configuration to whatever sonatype says (#6013)
0d083426df4 Try specifiy explicit sonatype endpoint to publish jars to to be compatible with the changes in sbt ci release 1.11.x (#6012)

## chain

## cli

## clightning rpc

## Core

This release improves support and fixes bugs for handling Taproot transactions.
It also improves performance of core.

2a8b4653770 core: Add a type to the taproot annex (#5999)
c0a1219362f core: Add support for testnet4 (#5945)
2dd140ad599 core: Add TxSigComponent.spendingOutputs (#5957)
c999084d970 refactor: Rename TxSigComponent.output -> TxSigComponent.fundingOutput (#5956)
fb49548e217 core: Fix `TapscriptControlBlock` bugs (#5955)
6d240d5c540 core: Add `TaprootWitness.sigVersion` (#5954)
6287e412819 core: Change type from `Byte` -> `LeafVersion` in `TapscriptControlBlock` (#5953)
32eaf31e48b core: Add ADT for `LeafVersion` (#5952)
aeca55f5efa core: Fix type annotation on `TapscriptControlBlock.apply()` (#5951)
ed66937d01b core: Fix bug when converting OP_0 to long (#5948)
959993bed71 core: Add Address descriptors from BIP385 (#5935)
62252daff80 core: Optimize base58 serialization and deserialization (#5934)
124451f124b core: Add carveout for parsing 'nonstandard' string to ScriptType.NONSTANDARD (#5933)
0c8f86545e5 core: Tighthen `ScriptFactory.isValidAsm()` checks (#5929)
12c1bdfe09b core: Tighten `MultiSignatureScriptPubKey.isValidAsm()` check (#5928)
b291e6d31a1 core: Use splitAt() rather than duplicitvely slicing in ScriptParser.sliceConstant() (#5927)
31fbadaeb38 core: Remove ParsingHelper inside of ScriptParser (#5926)
adfeb7fd041 core: Use `ByteVector.concat` in `RawTransactionInputParser.write()` (#5925)
2a0b2e6238c core: Use `ArrayBuffer` instead of `Vector` inside of `ScriptParser.parse()` (#5922)
3c148ebf48c core: Don't unnecessarily encode to hex when parsing pushdata ops (#5923)
ec00e2218fe crypto: Add buffer size calculation to efficiently use memory (#5919)
a1dc9fb59ad core: Optimize Int64.fromBytes(), add invariant that WitnessTransaction.fromBytes must have at least 4 bytes left over for the locktime (#5914)
8954e244220 core: Check `maxPushSize` limit inside of `P2SHScriptSignature.isValidAsm()` (#5921)
16e2ea2e926 core: Check if P2SHScriptSignature is push only when type checking (#5920)
c96df6a5ebb core: Optimize WitnessCommitment.isValidAsm() (#5918)
db06873634d crypto: Cache ECPublicKeyApi.isFullyValid (#5916)
618338b636f core: Use ByteVector.concat() to create transaction serialization (#5910)
752a7580b90 core: Make `BytesUtil.writeCmpctSizeUInt()` faster by avoiding `BigInt` allocations in `CompactSizeUInt` (#5909)
45470521118 core: Optimize writing of block headers to avoid unncessay array copies (#5908)
6f9efafa915 core: Add ScriptNumber check in P2SHScriptSignature.isRedeemScript() (#5900)
73cf14d03e3 core: Optimize ScriptParser.parseOperationByte() (#5899)

## Crypto

This release removes the requirement that data being signed be 32 bytes in length. 
Arbitrary data lengths can now be signed in accordance to BIP340.

45fbbd9b255 2025 03 15 Remove `data.length == 32` requirement from secp256k1jni (#5961)
74d8b59c9b8 crypto: Avoid recomputing ECPublicKey by caching it (#5915)

## db commons

## DLC node

## DLC Oracle

## DLC Server

## DLC wallet

## Eclair rpc

cda49a1fe40 Upgrade eclair to 0.11.0 (#5894)

## gui

## fee rate

## keymanager

## Lnd rpc

8331c6d10e0 lndRpc: Upgrade lnd to 0.18.4 (#5895)

## Lnurl

6511addcf34 lnUrl: Remove LnUrl module (#5917)

## node

c2f957b3a9a node: Fix bug where we were circumventing bitcoin-s.node.enable-peer-discovery (#5962)
c6d83199bfe node: Log receiving control messages (#5947)
f6820d1b409 node: Log sending version message (#5946)

## wallet

## scripts

6d7f53d375c scripts: Rework scanBitcoind() to take a Sink parameter to stream scan results to (#5981)
bf3fc65db34 scripts: Update logback.xml (#5898)

## testkit-core

## testkit

## tor

## Website / Deps

97d2eb298d Update metrics-core to 5.0.4 (#6057)
e3d7b548c9 Update metrics-core, metrics-healthchecks, ... to 4.2.36 (#6056)
47b9a1705e Update pekko-actor, pekko-discovery, ... to 1.2.0 (#6058)
0d049f6e00 Update sbt, scripted-plugin to 1.11.5 (#6053)
4cc6e8c47c Update metrics-core, metrics-healthchecks, ... to 4.2.34 (#6052)
edbe2cda71 Update scalapb-runtime to 0.11.20 (#6051)
09e716c1d7 Update sbt-unidoc to 0.6.0 (#6050)
cbc4a44189 Update sbt-native-packager to 1.11.3 (#6049)
5eddf19a36 Add 1.9.11 website version (#6046)
c378cfdb52c docs: Update website deps (#6045)
88740c80416 docs: Update datadir location for macOS in documentation (#6044)
d614de7339f Fix typos and spelling errors in documentation (#6025)
8827c37c852 Fix typos and spelling errors in documentation (#5990)
7f849446673 fix: typos in documentation files (#5913)
349f78f4580 Update metrics-core to 5.0.2 (#6047)
d9001506e88 Update sbt-ci-release to 1.11.2 (#6042)
6b3c5109561 Update scalamock to 7.4.1 (#6040)
4d71d836c8d Update sbt-bloop to 2.0.13 (#6039)
79a88779a04 Update sbt, scripted-plugin to 1.11.4 (#6036)
66efeaeccd3 Update sbt-bloop to 2.0.12 (#6034)
f3ddc794072 Update pekko-actor, pekko-discovery, ... to 1.1.5 (#6030)
93295468576 Update sqlite-jdbc to 3.50.3.0 (#6031)
04174784591 Update typesafe:config to 1.4.4 (#6028)
4fa230e173b Update scodec-bits to 1.2.4 (#6029)
487d873e9ea Update sbt-mdoc to 2.7.2 (#6026)
3d6334b0fb0 Update scodec-bits to 1.2.3 (#6027)
c5bd36b7d55 Update sbt, scripted-plugin to 1.11.3 (#6024)
01ac00372f7 Update scalamock to 7.4.0 (#6023)
8342ab06e0d Update play-json to 3.0.5 (#6019)
fce206b59b1 Update scodec-bits to 1.2.2 (#6021)
1db02558485 Update sqlite-jdbc to 3.50.2.0 (#6022)
22c5744b898 Update scalapb-runtime to 0.11.19 (#6018)
d67d970534d Update metrics-core to 5.0.1 (#6016)
650a01a83fc Update metrics-core, metrics-healthchecks, ... to 4.2.33 (#6015)
498efba96d8 Update scalamock to 7.3.3 (#6017)
94a74476ac7 Update postgresql to 42.7.7 (#6010)
0e5d43fadea Update pekko-actor, pekko-discovery, ... to 1.1.4 (#6011)
23f6ada115e Update sbt-ci-release to 1.11.1 (#6004)
a89960643dd Update sqlite-jdbc to 3.50.1.0 (#6009)
d586fe2bd01 Update bcprov-jdk18on to 1.81 (#6006)
fa94207c0f2 Update sbt, scripted-plugin to 1.11.2 (#6008)
0022cb3a304 Update scalapb-runtime to 0.11.18 (#6007)
09b611ed780 Update metrics-core, metrics-healthchecks, ... to 4.2.32 (#6002)
dacedc5efd5 Update postgresql to 42.7.6 (#6003)
09cee015441 Update sbt, scripted-plugin to 1.11.0 (#6001)
21e90f3f60a Update pekko-http, pekko-http-testkit to 1.2.0 (#5997)
c275d82f307 Update slick, slick-hikaricp to 3.6.1 (#5996)
4fc7f078ee2 Update otj-pg-embedded to 1.1.1 (#5995)
5c0cfdef331 Update sbt-bloop to 2.0.10 (#5993)
b79a0de2630 Update scalamock to 7.3.2 (#5991)
2caa83fc075 Update guava to 33.4.8-jre (#5983)
b1c398cd192 Update gson to 2.13.1 (#5988)
1fcd773048f Update flyway-core, ... to 11.8.0 (#5989)
594c9fd5608 Update sbt-mdoc to 2.7.1 (#5984)
02d34ae2678 Update scalamock to 7.3.1 (#5986)
4d787a6672e Update sbt-scalajs, scalajs-compiler, ... to 1.19.0 (#5987)
f6dea7c8e86 Update flyway-core, ... to 11.7.2 (#5985)
f377006d45c Update sbt-mdoc to 2.7.0 (#5979)
21a08a4ac7d Update client3:core, ... to 3.11.0 (#5977)
8e442b6e197 Update flyway-core, ... to 11.7.0 (#5978)
1d07dc32f2f Update gson to 2.13.0 (#5980)
d834656bad8 Update guava to 33.4.7-jre (#5975)
ca4ff4b63c0 Update flyway-core, ... to 11.6.0 (#5976)
f96d39cea28 Update guava to 33.4.6-jre (#5971)
38505be3cdb Update scalamock to 7.3.0 (#5973)
220db1261c2 Update flyway-core, ... to 11.5.0 (#5972)
423b9807618 Update logback-classic to 1.5.18 (#5967)
5970ad5efa4 Update sbt-bloop to 2.0.9 (#5968)
94157cd141f Update guava to 33.4.5-jre (#5969)
67ef392e80c Update jna to 5.17.0 (#5964)
4bfc3a5e53f Update flyway-core, ... to 11.4.1 (#5970)
1546d423374 Update sbt, scripted-plugin to 1.10.11 (#5965)
15f79093764 Update metrics-core to 5.0.0 (#5963)
2de17466ff3 Update sbt-mdoc to 2.6.5 (#5966)
cbc28cbf017 Update slick, slick-hikaricp to 3.6.0 (#5959)
a085bcbbb53 Update sbt-ci-release to 1.9.3 (#5958)
06a36a43c16 Update flyway-core, ... to 11.4.0 (#5960)
ec65fe615cf Update sbt, scripted-plugin to 1.10.10 (#5950)
180138a4ae1 Update logback-classic to 1.5.17 (#5940)
9841b6329c0 Update flyway-core, ... to 11.3.4 (#5941)
4f441f052bb Update slf4j-api to 2.0.17 (#5942)
0e446390f27 Update sqlite-jdbc to 3.49.1.0 (#5938)
8c7528884d2 Update flyway-core, ... to 11.3.3 (#5937)
3fc49b6f7ca Update sbt-scoverage to 2.3.1 (#5939)
1f967aa14f0 Update flyway-core, ... to 11.3.2 (#5931)
9c563d87ddb Update sbt-mdoc to 2.6.4 (#5932)
64a3abaa1eb Update scalamock to 6.2.0 (#5911)
62a6a750cea Update sqlite-jdbc to 3.49.0.0 (#5912)
58d90219639 Update flyway-core, ... to 11.3.1 (#5907)
acddc9caab6 Update sbt-mdoc to 2.6.3 (#5904)
c1751285932 Update scala-collection-compat to 2.13.0 (#5903)
f40b7eea1da Update client3:core, ... to 3.10.3 (#5906)
7f5684d344d Update gson to 2.12.1 (#5901)
17c48facbdf Update sbt-native-packager to 1.11.1 (#5905)
af36b068f8b Update LICENSE (#5892)
6c54c01e2e1 Update sbt-coveralls to 1.3.15 (#5891)