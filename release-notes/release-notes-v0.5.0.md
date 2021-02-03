# v0.5.0 - The release who must not be named

## Running Bitcoin-S

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-0.5.0.zip` and then run it with `./bin/bitcoin-s-server` to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-0.5.0.zip` folder and start using the `bitcoin-s-cli` like this:

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
fingerprint `339A49229576050819083EB3F99724872F822910`

To do the verification, first hash the executable using `sha256sum`. You should check that the result is listed in
the `SHA256SUMS.asc` file next to its file name. After doing that you can use `gpg --verify` to authenticate the
signature.

Example:

UPDATE ME!!!!!!!
```
$ sha256sum bitcoin-s-server-0.5.0.tgz
aa1084edb5fcd3d1dbcafe0d0fba787abf4cd455bbe38809bd9a65a49c0cd0eb bitcoin-s-server-0.5.0.tgz
$ gpg --verify SHA256SUMS.asc 
gpg: Signature made Thu 24 Sep 2020 12:49:39 PM CDT
gpg:                using RSA key 339A49229576050819083EB3F99724872F822910
gpg:                issuer "stewart.chris1234@gmail.com"
gpg: Good signature from "Chris Stewart <stewart.chris1234@gmail.com>"
```

### Website

https://bitcoin-s.org/

### Releases

https://repo1.maven.org/maven2/org/bitcoin-s/

#### Snapshot releases

https://oss.sonatype.org/content/repositories/snapshots/org/bitcoin-s/

## BREAKING CHANGES to Configuration Options

It is required that all wallets pre #2217 need to have the configuration

> bitcoin-s.wallet.aesPassword = "changeMe"

See [issue #2245](https://github.com/bitcoin-s/bitcoin-s/issues/2245) for the errors that are thrown when this isn't set.

This password can be changed now with the `keymanagerpassphrasechange` RPC command.

## Multi Wallet Support

Initial support for saving multiple seeds and wallets has been added to Bitcoin-S!
This does not yet allow you to load and use multiple wallets at once, but you can switch between wallets using the
`bitcoin-s.wallet.walletName` config option.

## Module Level Changes

### Bitcoind RPC Client

The Bitcoind RPC Client now has support for v0.20 & v0.21!

The Bitcoind RPC Client now has support for using it with a bitcoind that has multiple wallets loaded, this is done by
setting the `walletName` parameter in the required functions.

#### Commits

940fce433d4 Fix rescans with bitcoind backend (#2605)

648e7d9abac Bitcoind v0.21.0 support (#2414)

3f18ba18d7e Bitcoind Version from String (#2421)

905491fe0e8 Fix getPeerInfo for v0.20 (#2402)

e8305477739 Use PSBT type in bitcoind calls (#2242)

aaf1a81d4ca Bitcoind RPC Multi-wallet support  (#2231)

7c887cc144b Move newer functions to common rpc interface (#2230)

a5621f5f565 Add bitcoind functions for load and unload wallet (#2229)

ead4eaa1477 Make bitcoind extend chain api (#2087)

206c5a93d00 Bitcoind v0.20 updated rpcs & tests (#2061)

0b100383f32 Create initial BitcoindV20RpcClient (#2060)

### Chain

The `ChainHandler` has been split up into two different types: `ChainHandler` and `CachedChainHandler`.

`CachedChainHandler` stores in memory the latest block headers and allows for faster syncing.
`ChainHandler` uses the chain database to access all data, this is necessary for things like the wallet to be able to
point to a single `ChainApi` and not get outdated data.

Neutrino syncing should be faster after some optimizations.

#### Commits

d159f3eb5fc Reduce number of rows selected by best filter header/best filter query (#2617)

1dacb74edf7 Optimize filter sync and fetching filter heights (#2568)

e44a620ea1a Small cleanups on the chain docs (#2515)

4e285e67469 Make ChainApi.processHeaders() return a failed future in the case we … (#2436)

9bef444fe70 Rework BlockHeaderDAO.chainTips into two methods: BlockHeaderDAO.{get… (#2443)

40f46d7c7a8 Remove BlockHeader.getBlockchainsFrom(), rework type signature for Bl… (#2431)

2de17bb4e47 2020 11 13 issue 2258 (#2260)

### CLI

New CLI commands have been added such as `decodepsbt`, `getblockheader`, and `bumpfeecpfp`. For a full list of available CLI
commands check [the docs](https://bitcoin-s.org/docs/0.5.0/applications/server#server-endpoints).

The `Cli` module is now published as a library, this should allow other projects to easily interact with a Bitcoin-S server.

The `cli` is also published using the `sbt-native-image` plugin. (#2494)

#### Commits

f849ba492f1 Wallet name in walletinfo (#2603)

94c71543fb9 Add createmultisig cli command (#2495)

fb43748d978 Version Number in logs & Cli Command (#2467)

2be2df12dbb Publish Cli as library (#2433)

847981317a0 Add getblockheader cli command (#2424)

f135322c094 Bump Fee Cli commands (#2415)

a2b54eef308 Import Seed cli commands (#2376)

ce9fbb0807a Analyze PSBT function (#2240)

c08379b2361 Decode PSBT function (#2237)

c662cf802d1 Make tx an argument for decoderawtransaction (#2155)

8c4308e2c6a Make cli pretty print json (#2130)

### Core

Fixed some bugs with PSBT parsing and adding witness scripts.

Initial BIP 155 support for sending and receiving addrv2 messages.

Initial BIP 325 support for interacting with Signet.

Experimental TLV support for DLC messaging, data structures, and algorithms.
These are subject to change and if your project is looking to use them
it is recommended to use [nightly builds](https://bitcoin-s.org/docs/next/getting-started#nightly-builds) to stay up to date.

#### Commits

0280707eaa5 Make contract info lazy so native image works (#2606)

c4edcb2d5f4 Update dlc before release (#2543)

04cc10effec Add better Block.toString that doesn't blow up logs (#2585)

810ca7c678c Create BlockSyncState type (#2567)

aa0a6f96b8b Bech32 address improvements (#2571)

abc1fdd23fb 2021 01 15 dlc refactors (#2518)

c4650e3930b Make allFactories lazy so we don't hit NPE exceptions (#2491)

056702f61c1 Add explicit signing version associated with a EventDescriptor (#2487)

223be805735 Limit bech32 addresses to segwitv0 (#2471)

141fc548bcb Modified rounding on DLCPayoutCurve interpolation to match spec (#2470)

aba109de16a Add attestation verification utility (#2438)

ff60c6e03e9 OutcomePayoutPoint now has the correct types and deffers rounding due to extra_precision in serialized points (#2441)

036d714563b Made TLV serialization and deserialization uniform under a succinct and expressive API (#2420)

13c4b0d9556 Added length prefixes to contract_info and cet_signatures TLVs (#2419)

719fab23b7a Remove CompatEither, it was needed for historical purposes to support… (#2394)

378c51991b9 DLC Data Structures on Master Cleanup (#2375)

49a281acaf0 Pulled down work from adaptor-dlc onto master (#2339)

49871df0c35 Require CJDNS starts with 0xFC (#2356)

3e7fade2240 Add latest ProtocolVersion (#2332)

13443fd0d1e Implement BIP 155 addrv2 messages (#2321)

b3d70f559a6 Fix TLV parsing for non-standard strings (#2312)

0c03aa9c24d Add function to validate OracleAnnouncementV0TLV's signature (#2308)

28c37ccf1fe trivial: Add a sha256 hash field on all TLV (#2300)

b638c6837e6 Move english word list to be represented by a Vector in memory (#2287)

18dfbed8c90 Add helper OracleEventTLVV0.maturation method (#2267)

e4194220c1e Update Oracle TLVs (#2185)

7178cb7136f Fixed P2SH(Segwit) bug and brought down DLC-used PSBT functionality (#2140)

ddf39057fe0 Only Validate PSBT BIP143 vulnerability on signing (#2204)

02c10fd89ed Pretty Fee Rate toStrings (#2210)

c275a8c8aa6 Make tx bytes functions lazy vals (#2180)

c5be81d5e21 Add address descriptors (#2176)

2e1b4d04919 P2SHTxSigComponent constructor now detects witness data (#2169)

7bbc89fb33f Optimize GetHeadersMessage.fromBytes (#2131)

4ac93660d2c Add BIP45 Multisig Purpose (#2103)

f69678d74bf Allow any HDCoinType (#2097)

fa31609daca Enable AddrMessageTest (#2106)

d158f4fcb08 Calculate HRP from network param instead of the inverse (#2079)

c2315082d8c Create ExtPrivateKeyHardened (#2073)

856f88f0ddb Initial SigNet support (#2057)

### Crypto

ECDSA Adaptor signatures have been added in a basic Java implementation.
These should be used with caution as they are experimental and still being developed.
They are based on the specification located [here](https://github.com/discreetlogcontracts/dlcspecs/pull/114),
however they do not fully follow the specification and will be updated in the future.

Low R Signing should now use the same algorithm as Bitcoin Core.
Previously our first attempted signature would use different added entropy.

#### Commits

2c2646c78d8 Commit add Opt/T fromBytes/fromHex methods similar to StringFactory (#2499)

e944ed083f3 Bump size of data for AesCryptTest (#2483)

313e9d6e798 Update secp branch with synced java files (#2448)

70d014f7b26 Fixed Low R signing (#2408)

c7e5c634f3a Brought down ecdsa adaptor signatures implemented in scala from the dlc-crypto branch (#2034)

5bcf3e2a533 Introduced NFC normalization for strings in CryptoUtil and added String hashing functions (#2102)

### Db Commons

Various improvements to initializing and parsing an `AppConfig`.

#### Commits

c91f81bdbfb Fix all DAOs to use safeDatabase (#2556)

0cc85cc0b3d Simplify DBConfig Test to fix failures (#2459)

00e6a81a2a6 Safely delete files in DBConfigTest (#2451)

6b2237d2e81 Fix overrides AppConfig.configOverrides with DLCOracleAppConfig, also remove hardcoded value out of
AppConfig.configOverrides so people have to implement it (#2209)

ca5324c6a22 2020 10 12 issue 2164 (#2173)

1997f22e7d3 Refactor db configuration to use the key 'user' rather than 'username' inline with slick documentation (#2184)

56b385a0e79 Create fromConfig functions for AppConfigs (#2170)

0e41c29a3fe 2020 10 08 issue 2147 (#2153)

da2f3232cd0 2020 10 06 explicit classloader (#2148)

475ed19df66 2020 10 05 redo config (#2121)

4aec33f38cc Move configuration for sqlite into the 'bitcoin-s' key (#2113)

### DLC Oracle

The DLC Oracle is a new module in Bitcoin-S!
The DLC Oracle allows you to attest to events and sign their outcomes for DLC participants.

The DLC Oracle can be used to as a standalone module or as an RPC server.
You can check out the [documentation](https://bitcoin-s.org/docs/next/oracle/oracle-server) to find out more.

This is still in beta and is likely to have some breaking changes in the future. As of this release, it is up to date
with the [DLC Spec](https://github.com/discreetlogcontracts/dlcspecs).

#### Commits

779eefb6a58 Rename createevent rpc to createenumevent (#2604)

89e3fc54f59 Fix oracle cli to use announcements (#2576)

7cce23abf72 Implement OracleAttestmentV0TLV, save outcome to db (#2516)

166440b34b4 Update DLC Oracle Signing Algo (#2465)

7ad477fdaa2 Don't allow negative outcome for unsigned digit decomp events (#2446)

0897ea5da17 2020 11 17 dlcoracle dbutil (#2272)

4c623a8fcb5 Fix listEvents in DLCOracle (#2265)

cc428498648 Use New Oracle TLVs in DLCOracle (#2162)

be6a2a21846 Add DbManagement tests for Oracle (#2186)

f55e83ae21e Oracle Announcement TLVs (#2149)

bc3c0af1632 Use Llyod's Oracle recommendations on commitment Signature (#2117)

4cc30e15a99 Don't allow duplcate or 0 outcomes for a DLCOracle (#2120)

fcc6558a651 Increase DLC Oracle test coverage (#2128)

5d56e95af7e Add basic DLC Oracle (#2094)

28ed8db9a58 Move DLC Oracle module to `master` (#2083)

### Eclair RPC

Eclair RPC is now updated to be compatible with v0.5

#### Commits

d2203f2359f Bump Eclair version (#2405)

e38569db105 Add officially supported version of bitcoind by eclair, also add the ability to specify which version of bitcoind you are using for EclairRpcTestUtil.getBitcoindRpc (#2490)

### Fee Provider

Fixed a bug with the BitGo fee provider that occurred during high fee environments.

#### Commits

a08fc0c8a24 Fix BitGo fee provider parser (#2381)

### GUI

The GUI's icon now changes based on the network of the running server. The GUI now will update its balance periodically.

#### Commits

827f0f3b6a8 Uniform GUI denominations (#2534)

95935dca34f Make GUI auto-update balance (#2197)

d2f08fd2269 Add network specific icons for bitcoin-s (#2067)

### Key Manager

The password to encrypt a seed as well as a BIP 39 password are both now configurable.
There also exists now the ability to change your seed's password.

The Key Manager now supports the ability to store an `ExtPrivateKey`
instead of a Mnemonic. This is only used when using the `importxprv` cli command.

#### Commits

f5dae427613 Make KeyManager return better error messages (#2464)

0e9449d9a18 Add ability to store ExtPrivateKey instead of Mnemonic (#2372)

bd3584eb43f Mask BIP39 password in the BIP39KeyManager (#2350)

79b3d959d3a Create KeyManagerAppConfig (#2268)

744d8d18ab5 Add ability to change aes password (#2254)

15bb9357696 Use same config option for key manager projects (#2252)

42e9cbb1f21 Make aesPassword option for wallet config (#2217)

7b72d82440d Make BIP 39 password a config option (#2234)

### Node

Fixed an issue where the node would not request witness data for blocks.

Fixed an issue with parsing unknown messages.

#### Commits

d608a442556 Ignore block messages while syncing (#2587)

ddd47b1cf10 Optimize `node.start()` and fetching filter & filter header heights (#2554)

7e942ba66dc Add unit test to make sure DataMessageHandler exception doesn't stop node (#2536)

da54d2e9fbd 2021 01 11 issue 2493 (#2503)

1ddd7bec1dd Recover errors in DataMessageHandler (#2460)

2b94b330528 2021 01 02 issue 2457 (#2461)

ecc4532bf7b Remove callbacks param from DataMessageHandler & PeerMessageReceiver (#2476)

238bd025952 Request filters after processing (#2463)

9aa4d0fcd1c Fail broadcasting transaction when disconnected (#2336)

9a5ba7bd4f3 Fix P2PClient parsing unknown messages (#2315)

7172b4a049b Request witness blocks from peers (#2289)

61c1b916113 Bump user agent to new version (#2055)

### Server

Can now use bitcoind as a backend for your bitcoin-s server.
Checkout [the docs](https://bitcoin-s.org/docs/0.5.0/getting-setup#option-b-bitcoind-backend) on how to set this up.

Fixed issues where the server wasn't always returning correctly formatted json.

#### Commits

db6cd10c2f1 Fix bestblockhash rpc call (#2612)

a42601ebacd Fix rpc bind address from config (#2542)

a1a2524b56e Use mainnet for default server (#2453)

93dae6c2394 2021 01 09 issue 2496 (#2500)

8e6a37e9880 Remove extra logback files (#2456)

7409bae8f9d Remove need for params in RPC request (#2418)

eaecc1c377b Implement rpcbind to allow for binding to a different interface (#2409)

de5041ee278 Fix server configuration for the app server (#2413)

42c15ba6726 Fix datadir config option (#2271)

55249669989 Add directive to configure timeout on the appServer (#2275)

37012819959 Have api endpoints return json (#2178)

720932f7975 BitcoindRpcConfig Test (#2181)

9036869990a Add test for server startup (#2171)

8577bfd34d8 Remove requirement for ZMQ with bitcoind backend (#2137)

30b6e226622 Fix NPE and log location on server start up (#2163)

6b98154622f Fix npe exception on oracleServer/run (#2160)

593f170f2a2 Refactor Mains to use common BitcoinSRunner (#2141)

8048f1e80c7 Add DLC Oracle Server Endpoints (#2105)

105260249f1 Bitcoind backend on server start up (#2088)

c853151206f Create Util functions for wallets with a bitcoind backend (#2076)

23d0d7e0eaa Create BitcoindRpcAppConfig (#2077)

### Testkit

The testkit now allows you to set any binary location for bitcoind and eclair.

#### Commits

4bf637a008b Fix 'client1.getDaemon.datadir.exists() was true' (#2544)

d5f162ee09d Fix BitcoindV21RpcClient testkit errors (#2533)

8c918ac0a72 Refactor test case to be more idiomatic in hopes this kills CI failures (#2524)

84ce69eac85 Clean up fixture shutdown code a bit to try and see if this resolves … (#2498)

48cb939976f Don't allow fee unit gen to be 0 (#2346)

cbd6ea2cbec Start refactoring testkit to allow for specifying a different binary … (#2325)

8f9f7750d90 Remove hard coded bitcoind version eclair was depending on and switch to latest version of bitcoind (#2324)

c29d95eeceb Make BitcoinSAyncTest more thread safe and make sure we have all the … (#2292)

### Wallet

[Multi-Wallet support](#Multi-Wallet-Support) has been added.

New wallet commands have been added like `signpsbt`, `listreservedutxos`, and more.

The wallet now stores its last sync height to allow for chainApis that are ahead of the wallet.

The wallet now defaults to use native segwit (bc1 addresses) instead of legacy addresses.

#### Commits

ee7c96245e9 Add walletinfo rpc (#2546)

cba90e5c2bb Fix rescan to fetch blocks during scan (#2540)

36b5fc14271 Create isChange function for wallet (#2535)

f3e81d027df Remove WalletSync.sync() -> WalletSync.syncFullBlocks() (#2522)

b76be736b83 Rename wallet.getSyncHeight() -> wallet.getSyncDescriptorOpt(). We don't just use height in the descriptor, the hash is just as valuable for connecting to chains (#2479)

1d701c48a3c Use written txDbs in TransactionProcessing (#2449)

90e2a4b6f17 Account for rounding of fee rate in CPFP test (#2423)

92ac986baad Add extra checks for RBF transactions (#2416)

89222eb7669 Add wallet function to bump fee with CPFP (#2399)

64a6b6bbc5b Add wallet function to bump fee with RBF (#2392)

2db21fc3d21 Add get transaction cli command (#2370)

901fb2af17a Rescan Improvements (#2379)

18755df3d17 Multi Wallet support (#2345)

d884f7b4e9b Have makeOpReturnCommitment use random UTXO selection (#2320)

641b2236d6a Let wallet sign PSBTs (#2236)

7530a138300 Add listreservedutxos cli command (#2247)

641538440f1 Fee Provider from config (#2219)

49a58133ec2 Add Wallet State Descriptors (#2157)

e2b01e17ba8 Small improvements on FundTransactionHandling (#2143)

f7b97ba36eb Use SubtractFeeFromOutputsFinalizer when sending full utxos (#2072)

b59a17def0d Add ability to fully spend utxos (#2063)

### Website & Documentation

865f1a6d460 DLC + Adaptor Docs (#2552)

8e799a4b245 Update website dependencies to address security warnings (#2607)

edf81251340 Hide chain api creation (#2569)

9f36250646c Add documentation to website for bitcoin-s-cli native image (#2591)

096504b905f Add enum oracle example (#2601)

61c3b8185e5 Add oracle configuration to example configuration.md (#2592)

0424bd8309a Fix aesPassword config in configuration docs (#2588)

ec65e48a29e Update Oracle Server docs, fix oracle server bugs (#2581)

d5243adf5a9 Add wallet sync documentation (#2565)

7c18083577c Remove range event example from website docs (#2575)

10c54186811 Updated DLC cli docs (#2551)

74884a50d27 Clarify getting-setup.md -- make distinctions between optional steps … (#2528)

f34f35ae2b7 Default to native segwit wallet (#2548)

663d94d1c0c Add comment about using defaultAccountType (#2549)

4a6c45f3137 Add whitepaper to website (#2547)

48918380003 Rescan/Restore wallet docs improvements (#2539)

65919e135b1 Touch up bitcoind docs (#2514)

1ea4e1b0be5 Add download link to navbar & downloads page (#2473)

78e28baf3c5 Rename v0.4 version to 0.4.0 in docs (#2478)

4cd3079d890 Fix docs sidebar (#2466)

3e9b6881d23 KeyManager Docs (#2426)

ef1a3e7baf5 Add doc about no mempool (#2429)

2610a23a234 Fix broken links in docs (#2439)

732c38486ee Add to docs we support schnorr (#2428)

3d37d919a36 Add supported Bips docs (#2427)

c33f88eb050 Add old release notes (#2385)

e5225993c3c Change CI Status to use Github Actions badge (#2364)

7f759ab0840 docs: Logback conf command line docs (#2270)

3314cd36d05 docs: Add DLC Oracle db conf to docs (#2269)

add6b495b3c Add release notes directory similar to what bitcoin core does, docume… (#2246)

881d3b0b781 docs: Oracle Server setup docs (#2187)

25bc97765ed docs: Clarify there are 2 options for node backend (#2177)

6db248de032 docs: Fix internal database configuration docs (#2156)

08ac5cc9f12 docs: Add Bitcoind Backend Docs (#2136)

0c31a9d3e69 Update slack links (#2134)

2882956f303 docs: DLC Oracle Server (#2125)

ffc42a1ed3b Server installation guide (#2091)

dd272a8d8e3 systemd script (#2074)

c857f5e6c7b Change versions in readme (#2065)

40db826ffc5 docs: Calculate correct hashes in dlc doc (#2059)

### Other

72db35112d3 Fix compiler warning (#2609)

2f00b4a5deb Update sbt to 1.4.7 (#2598)

f2172965221 Update scala-collection-compat to 2.4.1 (#2597)

409373b89db Update metrics-core to 4.1.17 (#2580)

4a1316263f8 2021 01 27 conectionpool (#2578)

3ebf786fcd3 Update website deps (#2573)

23f15dffc71 Update sourcecode to 0.2.3 (#2557)

7bb0b10cd5e Update akka-actor, akka-slf4j, akka-stream, ... to 2.6.11 (#2517)

93fea25c32e Fix native CI workflow for different branches (#2521)

a16018bf172 Update sbt-mdoc to 2.2.16 (#2541)

1903ee30b07 Update native-lib-loader to 2.3.5 (#2523)

d21657e18bb Update sbt-native-image to 0.3.0 (#2511)

0f84e3d158b Add new sbt-native-image plugin that helps generate a correct native … (#2494)

0c0e2090136 Update scalafx to 15.0.1-R21 (#2492)

27e50b2c34e Update javafx-base, javafx-controls, ... to 16-ea+6 (#2489)

9eb6ec84582 Update play-json to 2.9.2 (#2468)

663a9514301 Update LICENSE year (#2474)

cc2c438da19 Remove -Xfatal-warnings flag for tests (#2469)

b0b56dd5da2 2020 12 18 enable lint options (#2454)

4e862b72976 Move RPC server logic into separate project (#2440)

f7c6fc141d0 Skip CI tests for docs PRs (#2435)

19c68b77b97 Update sbt to 1.4.6 (#2434)

528b4e01ee5 Outstanding DLC branch diff (#2432)

41b1c4a88d5 Update bcprov-jdk15on to 1.68 (#2422)

b5d62161501 Windows Secp Update & fix for parsing Windows paths (#2398)

8ed30e72a8c 2020 12 20 root dependson (#2404)

bc6561465e0 Update scala-collection-compat to 2.3.2 (#2401)

bb4a2667d14 2020 12 19 enable test compileropts (#2400)

719ccbe9326 PoC: Attempt to enable parallel execution on CI now that we have github ac… (#2396)

0f037f3fb9d Remove autoScalaLibrary := false, this was not being used correctly and now is hindering builds on intellij (#2397)

0f56ab2113e Update scodec-bits to 1.1.23 (#2391)

307ba23d0d1 Update sbt-native-packager to 1.8.0 (#2382)

a080fc6730c Update scalacheck to 1.15.2 (#2378)

44d54ff5a52 Update sbt-mdoc to 2.2.14 (#2383)

ac374780c14 Ignore DLCPayoutCurveTest until issue 2369 is resolved (#2388)

c43e78adf3f Move CI into indiviual workflows (#2353)

35025f9843e Setup Github Actions (#2319)

4bfcaa22353 docs: Fix dependabot warning for highlight.js (#2328)

e6d8a02e42f docs: Replace Large Range with Digit Decomp (#2331)

a1862cb6489 Bump sbt to 1.4.5 (#2358)

696dce912d4 Bump ini from 1.3.5 to 1.3.7 in /website (#2349)

f3ce3619b04 Update scalafx to 15.0.1-R20 (#2348)

1e7e8473a30 Update sqlite-jdbc to 3.34.0 (#2347)

afeb992ca66 Update sqlite-jdbc to 3.32.3.3 (#2342)

cec0593929a Update scalamock to 5.1.0 (#2327)

92b7944e230 Bump scalac setting in secp256k1jni.sbt to scala 2.12.12 (#2317)

ebd917b363c Update sbt-bloop to 1.4.6 (#2316)

5927a2e2c2c Update sbt-ci-release to 1.5.5 (#2322)

fd08c98be09 Update javafx-base, javafx-controls, ... to 16-ea+5 (#2304)

f0d925fda17 Update scopt to 4.0.0 (#2298)

8836ed5e4fb Get Scala 2.13.4 compiling (#2294)

c5c7a423b29 Update scodec-bits to 1.1.22 (#2296)

b2fe48e9fbc Update sbt-mdoc to 2.2.13 (#2291)

d12c857441e Update scala-collection-compat to 2.3.1 (#2290)

9d842897f5b Update scala-collection-compat to 2.3.0 (#2285)

d0e3561bca9 Update sbt to 1.4.4 (#2288)

f22e1b6edb8 Update akka-http, akka-http-testkit to 10.1.13 (#2276)

76d4e36097d Update sbt to 1.4.3 (#2266)

ff9119e9757 Update spray-json to 1.3.6 (#2256)

ea232969068 Update sbt-mdoc to 2.2.12 (#2253)

35aae036f07 Update javafx-base, javafx-controls, ... to 16-ea+4 (#2249)

6538a14097b Update scalatest to 3.2.3 (#2250)

a6156cd2f3a Update scalacheck to 1.15.1 (#2243)

30cd7ed0b04 Update sbt-bloop to 1.4.5 (#2241)

497e33c1a39 Update scodec-bits to 1.1.21 (#2239)

3ba3ea61256 Update sbt-mdoc to 2.2.11 (#2238)

8a148357d56 2020 11 02 cleanup (#2233)

aca5e82bcd7 Exclude sbt keys that unused (are they really unused?) (#2194)

8346010027c trivial: Cache new modules for travis (#2190)

02eb3b8805a Update sbt-ci-release to 1.5.4 (#2235)

23a143d1950 Update bcprov-jdk15on to 1.67 (#2228)

8d379e39a0f Update sbt to 1.4.2 (#2232)

ac53a5ee637 Update javafx-base, javafx-controls, ... to 16-ea+3 (#2205)

c4970a3e3ec Update typesafe:config to 1.4.1 (#2212)

a679950ac52 Update sbt to 1.4.1 (#2202)

f75ca45e50e Update sbt-mdoc to 2.2.10 (#2199)

bc36519c06b Update postgresql to 42.2.18 (#2195)

77892490782 Update sbt to 1.4.0 (#2119)

b0b1f41ba01 Update akka-actor, akka-slf4j, akka-stream, ... to 2.6.10 (#2166)

708b160260b Update postgresql to 42.2.17 (#2168)

898a9df1dcc Update sbt-native-packager to 1.7.6 (#2172)

14cfa06a4bc Add -Xcheckinit copmiler flag to tests to try and find uninitialized vals (#2165)

c4bb3ac1218 Conslidate DLC tests into existing CI rows rather than having unique … (#2152)

6267512c4da Make ZMQ Listeners typed (#2144)

6ce7d3d7019 Upgrade to scalac 2.13.3 (#2115)

679691be27f Add signet db settings (#2068)

2abe5dff250 Update javafx-base, javafx-controls, ... to 16-ea+2 (#2038)

c2bf6dc4315 Update akka-actor, akka-slf4j, akka-stream, ... to 2.6.9 (#1993)
