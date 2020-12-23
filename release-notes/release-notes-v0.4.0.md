# v0.4.0 - Wallet Extravaganza

## Running Bitcoin-S

If you want to run the standalone server binary, after verifying gpg signatures, you can `unzip bitcoin-s-server-0.4.0.zip` and then run it with `./bin/bitcoin-s-server` to start the node. You will need to configure the node properly first, you can find example configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can also unzip the `bitcoin-s-cli-0.4.0.zip` folder and start using the `bitcoin-s-cli` like this:

```bashrc
./bin/bitcoin-s-cli --help
Usage: bitcoin-s-cli [options] [<cmd>]

  -n, --network <value>    Select the active network.
  --debug                  Print debugging information
  --rpcport <value>        The port to send our rpc request to on the server
  -h, --help               Display this help message and exit

```

For more information on what commands `bitcoin-s-cli` supports check the documentation, here is where to start: https://bitcoin-s.org/docs/next/applications/server#server-endpoints

## Verifying signatures

This release is signed with [Chris's signing key](https://bitcoin-s.org/docs/next/security#disclosure) with fingerprint `339A49229576050819083EB3F99724872F822910`

To do the verification, first hash the executable using `sha256sum`.
You should check that the result is listed in the `SHA256SUMS.asc` file next to it's file name.
After doing that you can use `gpg --verify` to authenticate the signature.

Example:
```
$ sha256sum bitcoin-s-server-0.4.0.tgz
aa1084edb5fcd3d1dbcafe0d0fba787abf4cd455bbe38809bd9a65a49c0cd0eb bitcoin-s-server-0.4.0.tgz
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

## PostgreSQL Support

Bitcoin-S now supports PostgreSQL as a database backend! By default Bitcoin-S still uses SQLite, but you can follow the [instructions here](https://bitcoin-s.org/docs/next/config/configuration#database-configuration) to use PostgreSQL instead. This can be used for every database's backend: Node, Chain, Wallet.

## BIP 340 Schnorr Support

Initial support for BIP 340 style Schnorr signatures has been added.
This support gives new data types as well as the ability to create and verify Schnorr Signatures.
The signing and verification is done in Java, it is planned to give the option to use bindings to secp256k1 in the future.

## Dropped Support for scala 2.11

Bitcoin-S no longer supports scala 2.11 due to akka no longer supporting scala 2.11

## Module level changes

### Bitcoind RPC client

A `BitcoindRpcClient` can now be used as a `NodeApi` and a `ChainQueryApi`, this allows test cases to be simpler as well as allows a wallet to be paired to a bitcoind. Other minor bug fixes and test optimizations are included as well.

#### Commits

dccc2b469c4 Fixed getblockchaininfo for v19 (#1711)
54c02e570c0 Fixed getAddressInfo for versions 18 and 19 (#1679)
088d9cb3d08 Add signrawtxwithwallet bitcoind function (#1665)
6556c536b93 BitcoindV17RpcClientTest get system time closer to getNewAddress call (#1612)
fac75a8adab Bump spread for address info tests in BitcoindV17RpcClientTest (#1591)
003bda2fe49 Attempt to fix BitcoindV19RpcClientTest from losing connection (#1515)
b621412f32f Optimize and Clean up WalletRpcTest (#1511)
35141012ec8 Implement Bitcoind as ChainQueryApi & NodeApi (#1481)
af02cb739ae Implement BitcoindRpcClient as a FeeProvider (#1486)
87af3795ea7 Attempt to add a bit of delay on expecting rescan to fail so we dont' have as much of a async issue (#1381)
5dddd82af8e Add JsonParseException catch clause in the isStartedF method (#1334)

### Commons

This is a new project that holds common items used throughout Bitcoin-S. Currently, it primarily contains JSON models and serializers.

#### Commits

e504d8dfb7d Added DLC json data structures (#1575)
06db27af592 Added appCommons to project aggregate so that it can get published (#1347)
e5ef17f0207 Move Json Reader, Writers, and Serializers to App Commons (#1328)

### Core

`UTXOSpendingInfo` has been replaced by `InputInfo` and `InputSigningInfo`, this is allows for better distinction between inputs with the ability to sign.

`TxBuilder` has been completely refactored to be more modular and interactive, [checkout the new docs](https://bitcoin-s.org/docs/next/core/txbuilder) for more information.

Signing now requires having the full funding transaction for due to the [BIP 143 vulnerability](https://blog.trezor.io/details-of-firmware-updates-for-trezor-one-version-1-9-1-and-trezor-model-t-version-2-3-1-1eba8f60f2dd).

Various bug fixes, optimizations, and improvements.

#### Commits

7c4822f67aa Have RawTxSigner use low R signing (#1722)
98cb6f78c7f 2020 09 18 btchrp stringfactory (#2031)
d7983538138 Updated Bouncy Castle Schnorr implementation to most recent BIP 340 (#2025)
24dfa7200af Create FeeUnitFactory and functions to calculate tx fee rates (#1990)
739288fef53 Cache DoubleSha256Digest.hex (#1932)
a9ad927a9c9 Apply string factory to a bunch of easy things (#1891)
ef9bd4165d8 Add new hash preimage PSBTInput types (#1893)
2a1c3998385 Implement basic TLV functionality (#1847)
d65f68ea217 2020 08 14 issue 1829 (#1833)
fa388c74484 Create more of a project structure in org.bitcoins.core.api, move DbR… (#1799)
d31806f76f7 Add synchronous version of StartStop (#1771)
e4e81d85461 Allow NonWitness and Witness UTXOs in same InputPSBTMap (#1761)
82ab96c54dc Make ECDigitalSignature extend NetworkElement (#1750)
8f86c180760 Improved block parsing time by approx. 33% (#1728)
5b2ad821ad2 Add low R signing (#1342)
5a75b326f47 Shuffle Finalizer Tests + bug fix (#1680)
7abca65ac45 Create ShuffleFinalizer (#1660)
18c931e210d Create BIP 69 finalizer (#1661)
8830eddda11 Segwit Signing Bug Fix (#1670)
a7c51e71790 Move BIP 143 invariant for PSBTs to allow signed transactions (#1631)
7f4a984a04f Override correct equals functions (#1621)
a309694f11f Only fail CallbackTest if promise is not completed (#1607)
a2ce5e5a8c7 Rework signing logic to take full funding transaction (#1560)
24f60c0833b ScriptSignature from P2WSH Witness (#1605)
1eadf098c94 Made P2SHScriptSignature.isP2SHScriptSig less forgiving (#1594)
de048e448c1 Refactored to allow support and disabling of RBF (#1588)
38a73de48f3 Lots of misc. improvements from dlc branch (#1583)
fb993078678 Added BlockTimeStamp abstraction for BlockStamps that aren't hashes (#1573)
041f805730f Callback rework (#1542)
9dea4254be3 2020 06 09 addr invoice string factory (#1538)
ad6d4a33ca9 Add StringFactory trait (#1537)
54c8a3feba5 Make Golomb Filter Test run in parallel (#1512)
7fd36e93117 Reduce number of hashes used in GolombFilterTest to keep CI from timing out (#1494)
afbce5bc4ae Cache numbers between 0 and 256 to make serialization/deserialization faster (#1482)
13884f54700 Rework Standard Finalizer (into pieces) (#1473)
1f4227c75cd Add more optimizations, MultSignatureScriptPubKey.isMultiSignatureScr… (#1475)
8b62272cb9b Fixed sequence computation for P2PKWithTimeout and RawTxFinalizer composition discovered during DLC rebase (#1461)
aa885688fd2 Implement FeeUnit SatoshisPerKW (#1455)
7df84519920 Fix fee rate calculation bugs, add tests (#1454)
01c2759e180 Attempt to optimize block tests more to avoid timeouts (#1459)
fec601c8f52 TxBuilder Refactor (#1426)
7dd1084321c Small optimization for ScriptOperationFactory.operations (#1450)
5c7585c6297 Fix allowing BytesToPushOntoStack(0) (#1448)
29579b52a3c Reduce number of property based tests from 200 -> 100 so we don't time out on CI (#1447)
5ff71181794 2020 05 17 optimizations (#1435)
f3469f8e288 Remove the clause in Transction.fromHex() where we throw in the case … (#1431)
aee88684f2e InputInfo Refactor (#1400)
1cf7e2191de 2020 05 11 bech32 addr tostring (#1413)
8514d361b19 Fix isMinimalPush to not look for OP_1Negate when pushing 0x4f(79) (#1367)
f6c799c9cfb TransactionSignatureChecker bug fix and tests (#1341)
19afc6be2c4 Removed SingleSigner abstraction and replaced with a simple signSingle method in SignerUtils (#1308)
4827d2dbe46 Remove flaky gcs test that times out (#1301)
14535fcd772 P2WSH Signer fix + tests (#1300)
8c953c3ad75 2020 03 28 uint32 cache (#1279)
752685fa4b7 Replace all usages of List inside of ScriptParser with Vector (#1280)
8ec143d0d41 Give `ScriptInterpreter` functions to verify a transaction or single input (#1223)

### Chain

The `ChainHandler` now picks the best chain based off of the one with the most work, instead of the longest.

The `ChainHandler` also now batches the processing of filters to make an IBD and syncing much faster.

Various other bug fixes have been done as well.

#### Commits

5df27a4395e Increase chain test code coverage even more (#2028)
314afbd538e Increase chain code coverage (#2023)
d0abc19ef5d Handle the case where our block headers are synced already (#2021)
2f6ff1d9557 Make ChainApi.getHeadersBetween() be inclusive on the 'from' parameter (#2009)
449e205b8b3 Introduce 'FilterSyncMarker' to ChainApi, make it clearier what exact… (#2003)
72cfd4bd4c3 Make BlockHeader, BlockHeaderDb have better toStrings (#2007)
7b4b4a290f6 Resolve issues with reorgs and syncing filters (#1969)
8d1d0fcba82 Update callbacks for LN support (#1938)
19bbe25197a Segregate mainnet chainhandler tests and regtest chainhandler tests (#1988)
f14eeb3a6c2 Optimize findClosestToTime (#1959)
a7c06a11cba Add chainHash to ChainParams (#1972)
1b788d05be4 Move getBestFilterHeader() use the best chain by chainwork for determining the fitler header (#1964)
b36ad557dda Change maxBy and minBys to use maxByOption and minByOption (#1961)
893d036ab6b Rename ChainApi.nextHeaderBatchRange -> ChainApi.nextBlockHeaderBatchrnage (#1957)
45c11f25eca Fix max by for getBlockCount (#1951)
4d6d96c9e79 Fix getBestFilterHeader for headers 2016 or more blocks away from tip (#1943)
5909a57f093 Make sure both filter ehaders and filters are empty before sending fi… (#1936)
5785f0c1a84 Fix maxBy() exception in the case of empty Blockchain in ChainHandler… (#1934)
453e02b58b0 Implement getBestFilterHeader based on a number of block headers that… (#1926)
267cf06fa7a Fix log to output correct function (#1913)
4cdbeafb586 Move ChainApi to core (#1888)
b1b5f0ad8d7 Use database to calculate number of confirmations (#1789)
ff8f9226840 Batch and execute headers for chain work calculation test case, this … (#1837)
11e08718ffd Attempt to batch checking of headers in chainTest to avoid reject exe… (#1826)
4323cd7048f Drop older headers on chain update (#1763)
576d4559240 Add back chain index after creating a temp table (#1753)
f4d4f8e8b9f Batch add filters to database (#1725)
5e80795c77b 2020 07 25 optimize recalc chainwork (#1697)
c3b7629e517 Fix Postgres types (#1723)
930f184dca6 Optimize getBestFilter functions (#1715)
17e9c87cbcf Fix getBlockchainsBetweenHeights (#1710)
383aaa76392 Remove need to parse every header in a blockchain on instantiation (#1704)
4b710dd0cab Create getBlockchainsBetweenHeights for BlockHeaderDAO (#1703)
8f3bcbb949a Add extra logs, fix best filter bug (#1624)
94568aba22e Change ChainApi.getBestFilterHeader() return type to Future[Option[Co… (#1550)
7d970eaf138 Attempt to fix memory leak in recalc chain work (#1535)
43a5c6c05f5 Fix Chaindb chainwork Migration (#1518)
31807c9cbd3 Implement best block hash correctly (#1452)

### CLI

The CLI commands now have proper help messages with the addition of a bunch of new commands. Checkout [the list of all the commands](https://bitcoin-s.org/docs/next/applications/server#server-endpoints) for more information.

#### Commits

667d2bc0e56 Fix case of cli command sendwithalgo (#1835)
1f6e2249ea4 Address Tagging/Labeling Support from CLI (#1790)
1c2bc7081cb CLI: Handle non-json responses (#1749)
a84543cff5a Pulled down dlc CLI code (#1589)
be37fda2e3d Add sendrawtransaction CLI and Api commands (#1351)
c7a350d8f31 Add CLI commands for current wallet apis (#1291)
380ef24d9e3 Fix CLI commands (#1271)
a7af3cd81da CLI command to stop node (#1268)
bbe7b78c4ca CLI help message include commands and their arguments (#1254)

### Crypto

This is a new module that used to be contained in the `Core` project. The `Crypto` project is meant to handle using cryptographic primitives (keys, signatures, and hashes).

Schnorr Support has been added! Schnorr keys, nonces, and signatures can be now be used inside of Bitcoin-S through libsecp256k1 or our own BouncyCastle implementation. This is in its very early stages as an alpha release without thorough review and is subject to change (as the schnorr BIP is subject to change). Please use with caution.

#### Commits

bed34f453e3 Use safeRewind for signWithEntropy (#1774)
2c78d9ff0da Create safeRewind() helper method in secp256k1jni (#1546)
665b5850021 Schnorr Data Structures (#1564)
2199cfbb287 CryptoContext Refactor (#1469)
a37a7d16291 Implemented (lax) signature parsing in Bitcoin-S (#1446)
4d9692f61e5 Crypto Project Refactor (#1380)

### Db Commons

[PostgreSQL support](#postgresql-support)!

Logging has been completely redone to only use grizzled slf4j.
Changing settings in your `bitcoin-s.conf` file will no longer effect logging, only a `logback.xml` file will.

Various bug fixes, optimizations, and improvements for other modules.

#### Commits

8361ff6e086 Refactor logging to only use grizzled slf4j (#2019)
f4711198804 Create DatabaseDriver ADT instead of booleans (#1902)
9a1dfdfdbd5 Remove AppConfig.initialize() in favor of AppConfig.start() (#1907)
1c84dc330f1 Add an ability to one Postgres database for all sub-projects (#1897)
f702410be51 Db conf on pr 1877 (#1879)
350928beca6 Correcly use reference.conf file (#1869)
1d7793cc456 Only use appenders with no Logback conf (#1867)
9dc7b2c0dd3 Remove default false for useLogbackConf (#1816)
26a2529f50e CRUD Test suite + updateAll improvements (#1618)
7b8f17ade1d Added locks on start calls for loggers (#1713)
436396773d2 Fix All Loggers! (#1695)
6a1e4aea016 Add .transactionally to CRUD.updateAll(), CRUD.upsertAll(), CRUDAutoInc.createAll() (#1698)
03da22b3915 Fix length of BigIntMapper (#1651)
102e5775f70 AppConfig Start Interface (#1598)
586075e9f83 Temporary fix for update all on CRUDAutoInc Tables (#1596)
8c0e64e1016 Move slickDbConfig out of JdbcProfileComponent trait into AppConfig (#1510)
a9430c2d5a1 PostgreSQL support (#1315)
0a35cdb6ef4 Implement AppConfigFactory (#1462)
c811ccc62a7 Add option to correctly use logback config (#1398)
64b13846d9a 2020 04 08 multi db dao (#1355)
02d926bb256 Reduce default num threads used for slick being a thread pool of 20 to 5 (#1281)

### Eclair RPC

Eclair has been upgraded to `v0.4.1` and `EclairInstance` now recognizes more config options.

#### Commits

5426c01c9e7 Fix CI failure in eclair test (#1735)
15870a775b9 Eclair RPC 0.4.1 (#1627)
8350a47bdbf Upgrade Eclair to v0.4 (#1421)
848c4cd36a4 Add bitcoind and zmq config options to EclairInstance (#1428)

### Fee Provider

A new project for Bitcoin-S, used for retrieving fee rates. An initial implementation for getting fee rates from [bitcoiner.live](https://bitcoiner.live/), [BitGO](https://www.bitgo.com), and [mempool.space](https://www.mempool.space) are included. For more information checkout the [docs](https://bitcoin-s.org/docs/next/fee-provider/fee-provider)

#### Commits

f8ca35b1c04 Add mempool.space as a fee provider (#1751)
38c80d78906 Implemenet BitGo fee provider (#1662)
6ecb3cdbd31 Attempt to re-enable fee provider publishing (#1503)
89ec91f3f31 Skip publishing of fee-provider-test (#1495)
217a8b650a8 Skip publishing fee provider for now (#1492)
3e323ce18e0 Fee Rate Api Support (#1470)

### GUI

A new GUI has been added for the wallet. It has minimal functionality but allows for basic sending and receiving.
There is a tab for executing DLCs however it will not work without compiling from the `adaptor-dlc` branch, more information can be found [here](https://bitcoin-s.org/docs/next/wallet/dlc).

#### Commits

56937f77285 Give gui command line arguments (#1931)
bbb43c2687d Fix gui theme to correctly color the button bar (#1626)
4683d41568c Added DLC GUI stuff to a new package in the existing GUI and made a new tab for DLCs (#1590)
b0229090d1d Introduced bundle project and created main class that runs server and then gui (#1531)
ab9b0bd88af Remove GUI unused imports (#1440)
91b2602c70c Add bitcoin-s icon, add dark theme to dialogs (#1425)
afd67b62922 Modify gui background color to be same as our website (#1404)
55e3d2d6cef Dark mode GUI (#1316)
920199cf9e8 Constructed simple Bitcoin-S wallet GUI (#1285)

### Key Manager

A Key Manager now has a creation time, this should allow for more optimal rescans if used.

#### Commits

293eb242c1c Increase test coverage in WalletStorage (#2030)
efc8e7213a5 Increase code coverage in key manager (#2024)
9c17e001391 Implement KeyManagerLogger (#1386)
eb37e551e05 Increase Key Manager test coverage (#1465)
556f7135897 Add wallet creation time for rescans (#1353)

### Node

Some notable bug fixes in the `node` project:

- Safely handles reorgs that happen when the node is offline

- Can now safely rebroadcast transactions

- Does not need to wait for a new block to begin syncing filters

- Send correct the time in a `VersionMessage`

- Uses a more descriptive user agent

`NodeCallbacks` now return `Future[Unit]` to add the ability for the node to wait for the callbacks to complete, learn more reading the [callback docs](https://bitcoin-s.org/docs/next/node/node#callbacks)

#### Commits

e8c28de421c Warn if peer does not support services we need (#1921)
be891596b68 Improve logging in DataMessageHandler (#1922)
0f5929d65e8 Clean up calls in Node and Chain (#1896)
79f757311e0 Send get filters message if we haven't cached any yet (#1900)
c9c18fabc5e Create ADT for NodeType instead of booleans (#1901)
485874d2158 Simplify Transaction Broadcast (#1872)
098c0ee2f52 Update user agent to 0.4.0 (#1887)
9a36d791dc9 Fix safely broadcast a transaction twice test (#1851)
e2294414bfa Send correct version message on node start up (#1793)
97ddf62b9fe Send GetHeadersMessage using all of our cached headers to prevent reorgs from stalling node (#1758)
29c5bffb3cd Stop requesting merkle block messages while in neutrino mode (#1730)
7db465f2226 Callbacks to appconfig (#1740)
5776eaede4d Start syncing filters on node startup (#1729)
728a4a841f3 Fix Node Startup Issue (#1683)
c64a590d1a8 Skip downloadBlocks if given an empty Vector (#1690)
c65338feb6c Remove need for wallet from BroadcastTransactionTest (#1666)
2b919ff3571 Node broadcast tx twice test (#1611)
65c7c847621 Drop AutoInc col for Broadcastable Transaction Table (#1630)
f7efc25a425 Add tests that NodeCallbacks are executed (#1582)
b7504edf482 Have BroadcastTransactionTest rebroadcast on failure (#1561)
0928fcae463 Optimize UpdateBloomFilterTest (#1548)
4ee234d9997 2020 06 12 mv to appconfig (#1553)
aa53ee5f57f Fix and Optimize Broadcast Transaction Test (#1545)
e6af044e3cf Add more descriptive log message in P2PClient when we get disconnectd (#1514)
d0ad497232a Try and improve reliability of UpdateBloomFilterTest, also add getMem… (#1434)
38fe580c061 Neutrino Logging (#1382)
2194196e760 Re-enable NeutrinoNodeWithWalletTest for Linux (#1366)
831e89c72b0 Use FutureUtil.foldLeftAsync() to process messages we parsed on the p2p network. This moves the blocking with Await.result() from inside of each message we process, to after the entire batch of messages is processed (#1326)
064d8bd05e1 Fix MerkleBuffers test to be async (#1329)
efaf457d6a6 Make 'maxHeightQuery' vals inside of BlockHeaderDAO, CompactFilterHeaderDAO, CompactFilterDAO (#1325)
27dbefd2af8 Call getFilterHeaderCount async inside of nextFilterHeaderBatchRange while we are fetching our start height for fetching filters (#1327)
aa2d88f396d Network specific filterHeaderBatchSize (#1286)
717434dcaa5 Change NodeCallbacks to `Future[Unit]` s (#1206)
afd8d937213 Terminate not reliant on node.stop

### Server

Logging has been improved in the server, as well as some configuration options have been added, visit [the server config docs](https://bitcoin-s.org/docs/next/applications/server#configuration) to see them all.

The server should now properly handle errors on start up and shutdown.

#### Commits

e353d2181f2 Throw errors that occurr in startup (#1950)
6e2c8117893 Rolling log file location (#1846)
26f47d269c9 Fix server parsing for send to address (#1819)
a3a2248aa1c Add missing route for getaddressinfo (#1834)
60e82894d7f Don't wait for rescan completion to send message (#1836)
cad3c64cdab Output logger configuration on startup now (#1814)
47e305e1aae Config CLI option, datadir correctly read from config (#1807)
e247289c346 Make Akka log to file correctly (#1801)
d72316722d8 Bump stop timeout (#1797)
26aee5eb6ba Better return message for bitcoin-s-cli stop (#1777)
6f95c27aea9 Replace localhost with 127.0.0.1 automatically (#1772)
eb09319c273 Have AppConfig use BitcoinNetwork.fromString (#1748)
674c376dd74 Move where we call wallet.start() and node.start() to the same place to avoid initializaiton issue 1687 (#1689)
0ee735424c4 Start server before sync (#1682)
8b85751b461 Give more descriptive error when there are no peers set (#1652)
7784087bd56 Break things up in server/Main.scala, allow things to be done asynchr… (#1522)
c54ee108090 2020 06 05 mv chainwork calc (#1519)
29e439d202f Add rpcport configuration options in both bitcoin-s.conf and as a command line parameter (#1387)
4284f3e2a2e Log correct type of node sync (#1384)
ee2d74b272b Change sendrawtransaction return to be just txId (#1354)

### Testkit

Wallets created by `fundWalletWithBitcoind` will now have the same utxos as a wallet created by `FundedWallet`. This should make them interchangable and ease some pain from switching between the two kinds of test fixtures.

Wallet Fixtures now have a `bip39PasswordOpt` parameter to allow for testing some of the wallet's key manager.

Fixed a bug that led to performance issues due to a wallet's threads not being properly closed.

#### Commits

3e864383270 Rename parameter names from 'duration' -> 'interval' just like our ActorSystem.schedule() names the parameter (#2005)
b43d5d28f86 2020 08 22 chaintest cleanup (#1877)
e4460bb07f0 2020 08 21 clean broadcast dao (#1875)
50a43d0502b Remove uncessary extension of Async 'FixtureAsyncFlatSpec' in WalletDaoFixture, it's inherited from BitcoinSWalletTest already (#1881)
403d70eb0b0 Fix async bug with destruction of fixtures (#1878)
199661df7b5 Start calling appConfig.stop() in destruction fixture code (#1868)
c5f7c3d0e17 Disable logging for tests (#1839)
ac21e0418db Cap the amount of threads an actor system can spin up to 2 in tests (#1578)
a66ead6cbbf Make it so wallet fixtures take a bip39PasswordOpt as a paramter (#1555)
6b1973264e5 Fixed all walletTest threads not being closed (#1552)
977a696819c Testkit wallet with bitcoind uses bitcoind as api (#1499)
92ac40c97dc Fixed buggy type inference on Gen.frequency by explicitly passing in type parameter (#1439)
ce3e37d9c9b Create KeyManagerTestUtil.bip39PasswordNonEmpty for test case that requires non empty password (#1373)
260f52fe273 Make fundWalletWithBitcoind have the same utxo amounts as FundedWallet (#1364)

### Wallet

Many new APIs available for different types of sending and funding of transactions.

Wallet now has the ability to add callbacks for certain functions, check [the docs](https://bitcoin-s.org/docs/next/wallet/wallet-callbacks) for more information.

Wallets now use its `KeyManager`'s creation time for faster rescans.

Remove Unlocked vs Locked Wallet distinction, this was removed because it was not used, the `KeyManager` still has a locked and unlocked state however.

The wallet now has the ability to watch any `ScriptPubKey`, this will be useful for off-chain protocols.

Various bug fixes, optimizations, and improvements.

#### Commits

36b45790cfb Add ImmatureCoinbase TxoState (#2029)
ff878c532bd Use randomized fee rates for wallet tests (#1977)
bf1799d5734 Fix Two KeyManagers in scope for fundRawTransactionInternal (#1986)
2da714999db Don't spend immature coinbases, fix tests (#1981)
a4dc8053a2a Validate key manager matches account db on wallet startup (#1948)
bd94ff15f97 Only mark utxos as reserved on successful coin selection (#1944)
f734e002bec Calculate correct TxoState when processing a transaction (#1929)
4104e0c9732 Fix rescans that are larger than the batch size (#1916)
66ec89bcab0 Move WalletApi to Core (#1890)
28ff3186e44 Move WalletApi necessities to core (#1886)
acbdbfac418 Move all remaining wallet db representations (#1885)
f1b228a8ce1 Move TransactionDb to Core (#1849)
f7571206817 Move AddressTagDb to Core (#1850)
02f02fb22b3 Create KeyManager Api & move SpendingInfoDb to core (#1848)
d6975340e06 Fix selecting Utxos twice in fundRawTransactionInternal (#1866)
773dbb702ee Watch arbitrary SPKs (#1860)
0904ba4e4e3 Fix V8 Migration (#1862)
7c62bb285f3 Remove unnecessary parallelization in Wallet (#1823)
874a96eb1e0 Refactor wallet DB (#1798)
0ca5b692dc0 Fix get missing utxos (#1832)
c9fba8ad825 Create primary key of (address, tag_type) for AddressTag table (#1828)
86f68b3b622 Fix address tag issue where we weren't adding tags to an address, we were replacing existing tags (#1824)
c5617f6fbed Require utxos aren't spent when sending from outpoints (#1817)
cb962f4d957 Optimize unmarkUTXOsAsReserved (#1804)
1a8013242bc Fix for Wallet confirmed states (#1782)
d9024b17523 Have wallet shuffle inputs and outputs (#1721)
b044b6400d8 Implement Wallet.listTransactions() (#1744)
7db465f2226 Callbacks to appconfig (#1740)
d6ce8dee0b8 Move Node type of out Wallet API (#1708)
96ebf2b6a44 Wallet API remove app config (#1706)
d5a0bcd51e9 Wallet API, widen key manager definition, remove unlock functions (#1705)
68608199ceb Wallet API move execution context (#1707)
4ef425d3cb9 Add ProcessBlockTest (#1674)
24f83d28029 Create HDWalletApi (#1693)
8102fde681c Move function defs from wallet api to wallet (#1694)
3a1f3d3497d Unreserve spent utxos (#1676)
c6f1dcf6feb Fix CoinSelector for KiloX fee rates (#1664)
407c19bf019 Enforce unique outpoints for SpendingInfoDb (#1673)
9ed795718f1 Let Wallet find utxos by state (#1668)
15ddc74c258 Random Coin Selection (#1663)
c2fa7d7cc85 Address Tagging Attempt 2 (#1320)
021e21d61b3 Rescan when UTXOs don't have associated transactions (#1562)
fdb40263569 Formatting Fix (#1606)
ea62374fc60 Fix WalletAppConfig.hasWallet for Postgres (#1576)
e63061e9b9a Wallet callbacks (#1543)
1305e53f1a3 2020 06 14 wallet root accounts (#1556)
5d276d220f7 Attempt to fix database locking errors in process tx test (#1477)
5508af57a24 Create a simplified version of the WalletApi.unmarkUTXOsAsReserved() that just takes in a tx and scans outpoints if they are in our wallet, also move the mark/unmark methods out of Wallet.scala and into UtxoHandling.scala (#1463)
d6978e01bb7 Fix balance by account (#1457)
a1b220a4737 Add ability to unreserve utxos (#1458)
9172aa3206f Add ability to make OP_RETURN commitments (#1417)
c4382cddb62 Wallet Send with coin selection algorithim (#1409)
11fb182c10f Fix Warning in TransactionDb (#1427)
a9cd4450da5 Add list unused addresses call (#1408)
8f5c845a06f Add listFundedAddresses call (#1407)
bf6d90acfb7 Add listSpentAddresses call (#1406)
1454bf65522 Wallet send from outpoints (#1405)
4723dce744d Remove Unlocked vs Locked Wallet distinction (#1379)
20c6e43df24 Batch processing compact filters (#1363)
6cfe7b438b8 Ignore immature coinbase outputs when funding a transaction  (#1365)
5881aff1869 Rescan from account (#1348)
ee852bd3101 Remove redundant logic that computes an HDAccount. We don't need this because we are explicitly passing in the account we want to create (#1359)
8d4cbeb9c6c Require that addresses are the correct network when sending (#1332)
0a4ca6de986 Bump number of addresses generated in negative test case for AddressHandling where  we expect a illegal state exception to be thrown (#1333)
934b7319a88 2020 04 02 get new address queue (#1299)
c347fb5beba Fix Block Header Callback (#1331)
abd28f99622 GetAddress Wallet API call (#1287)
73b41460da4 Add wallet pay to many (#1317)
f620fb23197 getNewAddressHelper Refactor (#1322)
d6b4ac07a37 Update TxoState for transactions after they've been confirmed (#1178)
80882bf649e Wallet Transaction Tracking (#1197)
3ba5cae49b5 Fix hardcoded network for the default wallet account (#1277)
d8586ef85e6 Fetch addresses and utxos async in LockedWalletApi.processCompactFilter() (#1283)
0f899920f17 Wallet integration for nested segwit v0 spending (#1272)
90e4ca1cf35 Custom fee rate for wallet sends

### Website & Documentation

94c19035019 Update sbt-mdoc to 2.2.9 (#2033)
603e3db93f5 Update Website Deps (#2011)
09556a072d9 docs: Fix broken links in Docs + minor touchups (#1908)
b9095035ba7 docs: dlc branch -> adaptor-dlc branch (#1903)
e3452275619 docs: Remove e2e dlc info from documentation, add some notes about using gui bundled with node (#1905)
3805fc6286d docs: Updated setup and dlc docs (#1895)
9e5e2634f8b Fixed Transaction Signing doc (#1800)
f26d9e63d8c Docs: Remove WalletApi disclaimer (#1785)
83906d738ed Docs: Address Queue (#1776)
9cd2c41e3da Docs: Command line options (#1734)
4b03dcc4fab Fix nightly build docs section (#1639)
8ba3a797878 Use One click install in docs (#1640)
e3938544edd Fee Provider Docs (#1641)
15870a775b9 Eclair RPC 0.4.1 (#1627)
d9079678eb3 Update broken slack link (#1580)
9507315cd4d Document LN Data types (#1504)
95fb2b7e24b Add docs for server endpoints (#1505)
62ebf640d8b Add database configuration as a heading that can be hyper linked too (#1501)
c64c1ddfa6b Update docs pertaining to logging (#1471)
b3050c03adc Update stable version in doc.sbt, add getting started link to README.md (#1453)
9d59acbd9cd Fix spacing in README.md (#1444)
9a7b90c1328 Add DLC GUI Docs (#1438)
171a6c15ef7 Fix Oracle info in DLC doc (#1401)
c2c1c205181 DLC code snippet clarification (#1393)
14dfc92e34d Remove compile for dlc.md as we don't have schnorr in master (#1378)
e49bb86e473 Add fixed dlc doc instructions (#1376)
7b6c6c75e6c Add information on how to build libsecp256k1 to the secp README (#1318)
81b8ac7027e Added Tables of Content to the bigger docs using doctoc, updated signing-transactions.md (#1319)
445077338f4 Add google analytics key for bitcoin-s site (#1292)
7c30ecc3914 Add new GPG key (#1263)
4559edde07d Actually add all files for 0.3.0 on the website so they show up (#1256)

### Other

89fe847dbbd Update sbt-bloop to 1.4.4 (#1954)
23a8b1fbbd7 OSX native libsecp256k1 (#2014)
d819dd03223 trivial: add trivial phase to travis (#2008)
79ca91029c3 Update sbt-mdoc to 2.2.8 (#2001)
47fb9e0530d Update slick, slick-hikaricp to 3.3.3 (#1987)
4343a4b4deb Update scodec-bits to 1.1.20 (#1960)
e560159b54f Update play-json to 2.9.1 (#1994)
1b0113f1c49 Update sbt-mdoc to 2.2.7 (#1995)
149e5190f7a Fix RoutesSpec to assert results (#1979)
9daa7e73afa Bump scalatestplus version (#1894)
88ae8958954 Update scalatest to 3.2.2 (#1876)
f96bb97a9e7 Remove 2.11 specfic files (#1892)
8529eb7939e Cache Travis Builds (#1859)
68ec48c3873 Bring back strict compiler opts (#1854)
4186be51e00 Update scalafmt-core to 2.6.4 (#1686)
1660f560d44 Update postgresql to 42.2.16 (#1873)
b13d9ce7dc0 Update sbt-native-packager to 1.7.5 (#1838)
86647d3a324 Update postgresql to 42.2.15 (#1831)
90138a956de Update sbt-mdoc to 2.2.5 (#1812)
0bd60fd6d91 Revert scalaTestPlus 3.2.1.0 -> 3.2.0.0 (#1810)
1e41bf02e8d Update sbt-mdoc to 2.2.4 (#1786)
84dfb33c37e Update sbt-buildinfo to 0.10.0 (#1792)
0a16a79c75c Bump prismjs from 1.20.0 to 1.21.0 in /website (#1788)
931ad6b247d Revert scalaTest to 3.2.0 (#1791)
bb7786d433c Update akka-actor, akka-stream, ... to 2.6.8 (#1677)
5b385fdd175 Bump scala 2.12 compiler to 2.12.12 (#1669)
e9ec643daae Update akka-actor, akka-stream, ... to 2.6.7 (#1656)
cfdee8492ec Run only docs CI for PRs that start with Docs: (#1643)
1970f756fc7 Update sbt-native-packager to 1.7.4 (#1644)
34e9be12990 Update website deps (#1622)
d3641c3fff8 scalafmt On Compile (#940)
105052d877d Have travis only run tests if it compiles (#1577)
2b91c396162 Added compile scope scalafmtCheck to travis runs (#1563)
8d21cd13782 Bump akka to 2.6.4, fix deprecated things (#1374)
8241e129a9c Filter -Xfatal-warnings when using scala console so we don't get error messages when you are trying out code (#1485)
9040e2ccaf8 Filter warnings on doc for publishing (#1484)
11a635f3be7 Add -Xfatal-warnings on Scala 2.13 (#1483)
df13a0b3135 Drop 2.11 from inThisBuild.sbt and Deps.scala (#1479)
606d2fe29ab Add hardcoded jvmopts file that starts with a 2g heap (#1436)
920f0c3a201 2020 05 19 improve test performance (#1449)
a38b77d56ab upgrade Scala to 2.13.2 in places in travis.yml (#1432)
ddf060bcdd4 Bump .travis.yml scala version (#1368)
ce33c57ed90 Bump scala version to 2.13.2 (#1360)
28aea46e336 Increased test coverage by 0.97% (#1343)
abec5acccda Update Secp256k1 (#1310)
134075380bd Fix Travis caching (#1295)
9995e220ebb Bump sbt-native-packager to 1.7.0 (#1265)
b913a423553 Bump scalac 2.12.x series to 2.12.11 (#1257)
0c67805cee6 Update sbt-mdoc to 2.1.4 (#1244)
