# 1.9.10

This release is backwards compatible with the 1.9.x series of bitcoin-s

See the individual module sections for more information on lower level updates to the codebase.

Want to get started quickly? See our `docker-compose.yml` file. [See instructions here](https://github.com/bitcoin-s/bitcoin-s/#docker)

If you are a typescript developer, [you can access the backend via our typescript library](https://github.com/bitcoin-s/bitcoin-s-ts)

# Executive Summary

This release updates our RPC support, adds descriptors to `core` and updated our build to work towards support of scala3.

## Running bitcoin-s

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-1.9.10.zip` and then run it with `chmod +x ./bin/bitcoin-s-server && ./bin/bitcoin-s-server` to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-1.9.10.zip` folder and start using the `bitcoin-s-cli` like this:

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

## App server

## bitcoind rpc

## Build

## chain

## cli

## clightning rpc


## Core

## Crypto

## db commons

## DLC node

## DLC Oracle

## DLC Server

## DLC wallet

## Eclair rpc

## gui

## fee rate

## keymanager

## Lnd rpc

## Lnurl

## node

## Oracle Explorer Client

## wallet

## testkit-core

## testkit

## tor

## Website

## ZMQ

## Dependencies

6af79f9ed5 Update sqlite-jdbc to 3.47.2.0 (#5836)
788b99c184 refactor: Improve bitcoind wallet polling logic (#5834)
dee3a1cdee Update waffle-jna to 3.5.1 (#5832)
bab9fe0b2e Update scalamock to 6.1.1 (#5833)
1423f2c91e appServer: Add missing `server.stop()` in `BitcoinSServerMainBitcoindTest` (#5831)
479aee3dc6 docs: Run 'yarn upgrade' (#5830)
b9616066f2 wallet: Refactor {UTXOHandlingApi,AccountHandlingApi,AddressHandlingApi} to use 'get' prefix rather than 'list' prefix for method names (#5828)
43efce500d tests: Cleanup `BitcoindBlockPollingTest` producing ERROR logs during test fixture destruction (#5829)
7c3dbf96ca chore: remove redundant words in comment (#5826)
6b81211e37 Update client3:core, ... to 3.10.2 (#5827)
b1403155fc wallet: Refactor AddressHandling to be account specific (#5825)
051f6ed5cb Update jna to 5.16.0 (#5822)
1f4ba9c612 Update logback-classic to 1.5.15 (#5821)
ce18753462 Update sbt, scripted-plugin to 1.10.7 (#5823)
4d9c52c75c node: Move killswitch downstream to avoid queue deadlocks (#5819)
25517265fe wallet: Reduce noisy DEBUG log (#5820)
86a3b2f88c Update logback-classic to 1.5.14 (#5818)
916507ad00 Update sbt-ci-release to 1.9.2 (#5817)
97f807444a server: Deduplicate wrapping callbacks in steams (#5815)
3d16dfbc97 Update guava to 33.4.0-jre (#5814)
881a15f518 Update sbt-bloop to 2.0.6 (#5813)
54ef1bd0a5 chain: Add invariant that we have at least 1 header in Blockchain (#5810)
b709a58715 Update flyway-core, ... to 11.1.0 (#5808)
812f735110 node: Update DNS seeds, optimize `PeerFinder.start()` to start querying… (#5807)
d29dad4472 2024 12 11 issue 5625 (#5803)
eb6edab240 Add `NodeState.NoPeers`, cache network messages (#5800)
4df0a7bc4e Update flyway-core, ... to 11.0.1 (#5799)
06db0aa4b2 Update sbt, scripted-plugin to 1.10.6 (#5798)
e2a0617d67 wallet: Don't cache TransactionProcessing now that it doesn't have internal state (#5797)
f280f35431 Replace Future[Wallet] -> Wallet parameter in {BitcoinSWalletTest, BitcoindRpcBackendUtil} (#5796)
04757f9039 wallet: Remove `TransactionProcessing.blockProcessingSignals` (#5795)
e5ff1a5018 wallet: Fix noisy log, only log when we have an output to search for (#5794)
98e89a6e93 2024 11 27 processtransaction flaky test (#5793)
6f13f263ee wallet: Remove duplicate implementations of `SendFundsHandlingHandling.sendFromOutpoints()` (#5792)
960b172d26 Update metrics-core, metrics-healthchecks, ... to 4.2.29 (#5790)
7867d258e6 Update sqlite-jdbc to 3.47.1.0 (#5791)
dbdad48413 wallet: Remove unused wallet configurations (#5789)
e852b11dbc Update flyway-core, ... to 11.0.0 (#5782)
d1618a2277 Add unit test for RescanState to test recursive rescans correctly (#5786)
d5a77a2297 wallet: Batch database actions in processBlockCachedUtxos() (#5788)
ba8dd75312 refactor: DRY for `DLCWalletLoaderApi.loadWallet()` (#5787)
c6917b296b config: Rename config option bitcoin-s.wallet.defaultAccountType -> bitcoin-s.wallet.purpose (#5783)
dbe1e7b686 tests: Make `NeutrinoNodeWalletTest` more robust against spurious failures (#5784)
f6f01f333a wallet: Cleanup RescanHandlingTest (#5781)
345ab017e5 testkit: Randomize defaultAccountType (purpose) in tests (#5775)
795def555c Update flyway-core, ... to 10.22.0 (#5779)
e680c9d643 Update pekko-http, pekko-http-testkit to 1.1.0 (#5778)
fb318efe5e core: Fix bug where we weren't checking for valid hash types in `TaprootKeyPath.isValid()` (#5780)
b6cc97a663 2024 11 20 prevoutmap ordering (#5776)
d8ad023254 wallet: Implement taproot keypath handling in the wallet (#5772)
67bb3ceabd 2024 10 31 taproot signing (#5767)
80be2f5989 2024 11 15 partialsig typeparam (#5770)
bb0e40f05b 2024 11 16 rm isdummysig (#5771)
fc4802d4b0 core: Implement BIP86 (#5768)
cccaa582bd Add `Sign.{schnorrSign(),schnorrSignWithNonce()}` to `Sign` interface (#5754)
c5d57de618 core: Fix divergence in behavior between TransactionSignatureSerializer.hashForSignature() methods (#5765)
17f965fd45 2024 11 09 schnorrsig hashtype (#5764)
35fdb07e2d Update sbt-buildinfo to 0.13.1 (#5763)
340edc85b0 Update sbt-bloop to 2.0.5 (#5762)
d255914765 refactor: Use ECDigitalSignature.appendHashType() throughout codebase (#5761)
39e23c2a09 Update flyway-core, ... to 10.21.0 (#5759)
4e8d5ecc7f Add `Sign.{signWithHashType(),signLowRWithHashType}` (#5757)
21dd6fee80 Update sbt-bloop to 2.0.4 (#5758)
3569f51870 Update sbt, scripted-plugin to 1.10.5 (#5756)
aafac07491 Update sbt-buildinfo to 0.13.0 (#5755)
e69e1e5ad1 2024 10 31 digitalsignature (#5752)
e45860af90 Docs: README (#5751)
5187eee42a core: Terminate rescan early when RescanStarted.stop() is called rather than wait for the rescan to complete (#5749)
18c74c1351 Update bcprov-jdk18on to 1.79 (#5750)
471c063532 Remove rescan specific threadpool (#5746)
835c98e5b9 Fix race condition in `WebsocketTest`s (#5748)
3ae69b6ab5 wallet: Get `processBlockCachedUtxos()` using `DBIOAction` (#5740)
2366b80c70 Update sbt, scripted-plugin to 1.10.4 (#5743)
f75a52b521 Refactor `TransactionProcessing.processTransaction()` to use `BlockHashWithConfs` (#5744)
2521c5da0e Fix noisy log in TransactionProcessing (#5742)
91b3fc1e45 Update logback-classic to 1.5.12 (#5741)
fe8acbb42f 2024 10 25 Get `TransactionProcessing.processTransactionImpl()` using a single database transaction (#5739)
8917188220 wallet: Rework where we fetch the number of block confirmations for a tx in the wallet (#5738)
30663fe622 Remove scala version speicfic build code (#5737)
387bcfdeb8 Update flyway-core, ... to 10.20.1 (#5735)
ff459b674c Update sqlite-jdbc to 3.47.0.0 (#5736)
29f10d046c Turn on -Xlint (#5728)
e419b18d9c 2024 10 23 merkle vector (#5734)
07f17cfedf Rework Block and Transaction data structures to use Vector rather than Seq (#5733)
65e67287f8 2024 10 21 Replace `Future.sequence()` usage with `Future.traverse()` (#5732)
528ceae9d4 Update sbt-ci-release to 1.9.0 (#5731)
88125a3575 Remove .jvmopts file comments, they do not work on ubuntu (#5730)
28001af35e Add .jvmopts, conslidate JVM options there (#5729)
ed7a152a51 Update sbt, scripted-plugin to 1.10.3 (#5726)
04b9e6e75f Docs fix spelling issues (#5723)
dca2146647 2024 10 19 rm generic btree (#5725)
5f47fbe9ac Use BinaryTreeDoubleSha256Digest inside of Merkle.scala rather than BinaryTree[DoubleSha256Digest] (#5724)
613e19c281 Pull over docs/ from #5713 (#5722)
9e61e9bab7 Update flyway-core, ... to 10.20.0 (#5717)
7c07aa0355 Pull over simple syntax changes for {cli,oracleServer,bench,dlcNode,dlcOracle,dlcOracleTest,eclairRpc,lndRpc,lndRpcTest} from #5713 (#5721)
602725174f Pull over simple syntax changes for scala3 pt2 (#5720)
ab6d3f5cb7 Pull over simple syntax changes for scala3 libraries (#5719)
90203b295b Use default play-json json serialization macros for bitcoind json classes that cause issues with scala3 (#5718)
982090ed21 Fix typos (#5714)
b22e3d4aee Update client3:core, ... to 3.10.1 (#5716)
a0d6ecf50d Update logback-classic to 1.5.11 (#5715)
271da3ccea Upgrade upickle to 4.0.2 (#5710)
84aba7e349 Remove test case specific fixture setup in chainTest, revert to just using test suite specific fixtures (#5712)
00b1c85e65 Remove TableAutoInc[T] type parameter, its unnecessary and causes issues (#5711)
3ef88ecdc2 Update sbt-ci-release to 1.8.0 (#5709)
1e7e48d546 update flyway dependency to `10.19.0` (#5708)
f85953e527 2024 10 13 rm bitcoind v25 support (#5707)
38f0f4d692 2024 10 07 v28 bitcoind (#5696)
7dc58666b9 Update logback-classic to 1.5.10 (#5706)
4471f74ccf Add `error` field to`ImportDescriptorResult`, move `BitcoindException` to app-commons (#5705)
2d87129978 Reduce BitcoindRpcTestUtil.awaitConnection() interval from 10.second -> 1.second (#5703)
46d6256e39 Switch distrubution to temurin, use full semver (21.0.4) (#5704)
919789add2 Update sbt-assembly to 2.3.0 (#5697)
c7dce64718 Update sbt-ci-release to 1.7.0 (#5698)
5c73183046 Update logback-classic to 1.5.9 (#5700)
0843d7ecb5 Remove `-port` and `-rpcport` settings for `bitcoind` on startup for tests (#5702)
54646d4516 Update sbt-bloop to 2.0.3 (#5699)
7d601c45c8 Update metrics-core, metrics-healthchecks, ... to 4.2.28 (#5694)
bbac2590ff Bump to lnd 18.x (#5695)
e695474c61 Update client3:core, ... to 3.10.0 (#5693)
52c0625ba9 2024 09 30 dlcwallet has a wallet (#5692)
5afa058892 Update sbt-coveralls to 1.3.14 (#5691)
dd5a1af828 Update slick, slick-hikaricp to 3.5.2 (#5690)
e76768334d Update sbt-scalajs, scalajs-compiler, ... to 1.17.0 (#5689)
13a895efe9 2024 09 24 simplify wallet (#5685)
7caea21b6a refactor: Move more methods out of WalletApi (#5681)
58e2dc2b5a Update sqlite-jdbc to 3.46.1.3 (#5688)
67666b7579 Update waffle-jna to 3.5.0 (#5682)
c4b219d93a Update junixsocket-core to 2.10.1 (#5683)
e087b174c5 Move `RandomFeeProvider` to src (#5684)
d17934f17f Add `SendFundsHandlingApi`, remove `HDWalletApi` (#5680)
287bf984a0 Update sbt-bloop to 2.0.2 (#5677)
8cfd5e8d6b 2024 09 19 address handling refactor (#5679)
8c5d685953 Refactor codebase to have has-a relationship with `RescanHandling` rather than is-a (#5675)
a8e9dcd443 Update sbt-mdoc to 2.6.1 (#5674)
628c3f9bef Update sbt-bloop to 2.0.1 (#5673)
625e790477 Rework TransactionProcessing to be a has-a relationship rather than is-a relationship in the codebase (#5659)
6bf69de119 Update jna to 5.15.0 (#5668)
3525603f9e Update sbt-mdoc to 2.6.0 (#5671)
e2e604e8a0 Update scalacheck to 1.18.1 (#5670)
d6669aa061 Update sbt to 1.10.2 (#5669)
4befe4e70a Fix potential deadlock when offering to queue in managePeerAfterInitialization() (#5667)
cbccecf95d Add caveat for `ConnectPeer` logic to only attempt to sync from a peer when a query is timed out (#5666)
841762b841 cli: Fix log output for bitcoin-s-cli (#5664)
490e0217f1 Add payload to query timeout log (#5660)
7a8c6b8298 Update logback-classic to 1.5.8 (#5658)
9feabc1c2b Make `WebsocketTests` rescan test more robust (#5661)
d723610054 Update sbt-scoverage to 2.2.0 (#5656)
7d8dd2bc0a refactor: Provide DLCWalletUtil.verifyProperlySetTxIds() with the contractId (#5657)
c3288a7224 Update ant to 1.10.15 (#5653)
1939e9fd2e core: Fix bug in RescanStarted.entireRescanDoneF (#5654)
feeb2618d6 Move DLCDAOs to src (#5652)
b98087a5ee Update postgresql to 42.7.4 (#5648)
2d4a0adda4 Add FundTransactionHandlingApi, make FundTransactionHandling a case class (#5651)
24e22a48e7 Update sbt-scoverage to 2.1.1 (#5649)
42b13a6a62 wallet: Remove unecessary type parameter to TxCRUDComponent#TxTable (#5650)
14f456dec3 Update metrics-core, metrics-healthchecks, ... to 4.2.27 (#5644)
19571dd737 Update sbt-ci-release to 1.6.1 (#5647)
4212d6d616 Create `AccountHandlingApi`, move inheritance from `Wallet` into `HDWalletApi` (#5627)
364df59ea2 Update logback-classic to 1.5.7 (#5643)
21180213c6 Update sqlite-jdbc to 3.46.1.0 (#5646)
231397c409 Update scodec-bits to 1.2.1 (#5645)
2021f1f111 2024 08 13 walletholder appserver refactor (#5639)
f2726ec900 Update sbt-bloop to 2.0.0 (#5642)
a2855766c2 Update slf4j-api to 2.0.16 (#5637)
3d900f9b26 Update sbt-ci-release to 1.6.0 (#5641)
35003ca970 refactor: Change interval param of `BitcoindRpcBackendUtil.startBitcoindBlockPolling` (#5640)
41fab3dfd2 2024 08 07 `createnewaccount` rpc (#5638)
9c25209198 Update client3:core, ... to 3.9.8 (#5636)
0eb1788226 Refactor WalletApi.createNewAccount to not use KeyManagerParams (#5635)
f0bb99d9ef Update sbt-native-packager to 1.10.4 (#5633)
d1bff0eea2 Update sqlite-jdbc to 3.46.0.1 (#5634)
458f3cb7d3 2024 07 29 rm tx bitcoind callbacks (#5632)
38850d22e3 2024 07 31 bitcoind callbacks (#5631)
88b1dfd4c5 Update sbt-coveralls to 1.3.13 (#5630)
bc09757f8c Remove WalletHolder parameter from DLCWalletLoaderApi.loadWallet() (#5628)
f5adc331f1 Move WalletDAOs to src (#5626)
e68ebeadbc refactor: Create `UtxoHandlingApi`, move to has-a relationship within Wallet (#5624)
6b12bb515d Rework `NativeProcessFactory.cmd` to be `Vector[String]` (#5623)
4885bdb07d Add `-Xsource:3` to `lnd-rpc` (#5595)
559157e0c7 Update sbt-coveralls to 1.3.12 (#5620)
dcaebefe29 Update junixsocket-core to 2.10.0 (#5617)
7690fbe2a7 Update sbt-bloop to 1.6.0 (#5616)
27025e5dda Update sbt to 1.10.1 (#5618)
dc9e5ff1b9 Update sbt-mdoc to 2.5.4 (#5619)
a81119eb49 Update sbt-scoverage to 2.1.0 (#5614)
97a5758dfe Update sbt-mdoc to 2.5.3 (#5612)
f1e2238cfc Update play-json to 3.0.4 (#5611)
267f70860c Update otj-pg-embedded to 1.1.0 (#5610)
ca9bfa03e0 Update scalatest to 3.2.19 (#5613)
dad7e1cef6 Add support for bitcoind 27.1 (#5609)
29c38aa525 Update scala-java-time to 2.6.0 (#5606)
c559eca5f7 Update metrics-core, metrics-healthchecks, ... to 4.2.26 (#5607)
06295bb941 Update scalapb-runtime to 0.11.17 (#5605)
48462d4ac9 add checkout action to electron build (#5604)
9a002730f2 Update sqlite-jdbc to 3.46.0.0 (#5603)
677109c4ea Update sbt-bloop to 1.5.18 (#5602)
d13abaa8ea Update gson to 2.11.0 (#5600)
5833f5894d Update client3:core, ... to 3.9.7 (#5601)
1b109e7b36 Add previous tag to electron build steps (#5599)