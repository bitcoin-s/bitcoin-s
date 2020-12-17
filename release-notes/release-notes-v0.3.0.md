### Running bitcoin-s

If you want to run the standalone server binary, after verifying gpg signatures, you can `unzip bitcoin-s-server-0.3.0.zip` and then run it with `./bin/bitcoin-s-server` to start the node. You will need to configure the node properly first, you can find example configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can also unzip the `bitcoin-s-cli-0.3.0.zip` folder and start using the `bitcoin-s-cli` like this:

```bashrc 
./bin/bitcoin-s-cli --help
Usage: bitcoin-s-cli [options] [<cmd>]

  -n, --network <value>  Select the active network.
  --debug                Print debugging information
  -h, --help             Display this help message and exit
  <cmd>                  The command and arguments to be executed. Try bitcoin-s-cli help for a list of all commands

```

For more information on what comands `bitcoin-s-cli` supports you will need to look at source code as we do not have documentation yet, here is where to start: https://github.com/bitcoin-s/bitcoin-s/blob/v0.3.0/app/cli/src/main/scala/org/bitcoins/cli/ConsoleCli.scala#L35


### Website

https://bitcoin-s.org/

### Releases

https://repo1.maven.org/maven2/org/bitcoin-s/

#### Snapshot releases

https://oss.sonatype.org/content/repositories/snapshots/org/bitcoin-s/

## Migrations

All of our modules that require databases now have database migrations. The tool we use for these migrations is called [flyway](https://flywaydb.org/). To find your projects migraitons, you need to look inside of the `[project-name]/src/main/resources/[database-name]/migration/`. For example, the chain projects migrations live under the path `chain/src/main/resources/chaindb/migration/V1__chain_db_baseline.sql`.

Migrations can be executed by calling the [`DbManagement.migrate()`](https://github.com/bitcoin-s/bitcoin-s/blob/e387d075b0ff2e0a0fec15788fcb48e4ddc4d9d5/db-commons/src/main/scala/org/bitcoins/db/DbManagement.scala#L92) method. Migrations are applied by default on server startup, via the [`AppConfig.initialize()`](https://github.com/bitcoin-s/bitcoin-s/blob/master/db-commons/src/main/scala/org/bitcoins/db/AppConfig.scala#L49) method.

These migrations are setup so that project's databases and migrations are independent of each other. Therefore if you want to use the `bitcoin-s-chain` project, but not the `bitcoin-s-wallet` project, wallet migrations are not applied. It should be noted if you are using a module as a library, you are responsible for configuring the database via [slick's configuration](https://scala-slick.org/doc/3.3.1/database.html#using-typesafe-config) and calling [`AppConfig.initialize()`](https://github.com/bitcoin-s/bitcoin-s/blob/master/db-commons/src/main/scala/org/bitcoins/db/AppConfig.scala#L49) to ensure the entire module is initialized correctly.

## Module level changes

### Bitcoind RPC client

Bitcoin-S now supports the RPC commands introduced bitcoind v19 and is still backwards compatible with previously supported versions.

The list of new RPC commands can be found here: https://github.com/bitcoin/bitcoin/blob/master/doc/release-notes/release-notes-0.19.0.1.md#new-rpcs

#### Commits
6e5c0e4c8e Fix the time based test cases in BitcoindV17RpcClientTest (#1224)
858138fa85 Bitcoind v19 RPC (#910)
39bcfb22b6 Add sanity tests for .hashCode() and .equals() for multiple instances for EclairRpcClient/BitcoindRpcClient (#881)

### Core

Bitcoin-S has entirely re-written its signing logic in this release. Previously, `BitcoinUTXOSpendingInfo` was a generic type which was used for signing all `ScriptPubKey`s. Now `BitcoinUTXOSpendingInfo` has become an ADT with specific support for different kinds of signing and which supports nesting for nested `ScriptPubKey` structures such as `P2WSH` and `ConditionalScriptPubKey`. In conjunction with this change to `BitcoinUTXOSpendingInfo`, the `Signer` ADT has been re-written to work with specific `BitcoinUTXOSpendingInfo` subtypes and to support nesting via delegation to other `Signer`s when signing a `ScriptPubKey` with nested/composed structure. This change has allowed all signing logic to be removed from `TxBuilder` which now makes a simple call to `BitcoinSigner.sign`.

Alongside the refactor of all signing logic in Bitcoin-S, the ability to sign transactions with a specific key alone, and generate only a single `ECDigitalSignature` has also been added. This functionality is essential to multi-party transaction construction as Bitcoin-S signing previously required all `Sign`ers to be available at once for signing. This new functionality is carried out by `BitcoinSignerSingle`s using the method `signSingle`.

See https://bitcoin-s.org/docs/core/adding-spks for a full description of how `ScriptPubKey`s are now created and signed for.

Bitcoin-S now has implementations of [BIP 158 Golomb Coded Sets](https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki) in the new `gcs` package within `core`. This includes general functionality to create filters of any kind as descried in BIP 158, which can be found in `GCS.scala` and `GolombFilter.scala`. Specific support for [Basic Block Filters](https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#block-filters) used by version 0 Neutrino nodes is also implemented in `BlockFilter.scala`. [BIP 157 Filter Headers](https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#Filter_Headers) are also implemented in `FilterHeader.scala`. Various algorithms for matching against these BIP 158 filters are implemented in the `BlockFilterMatcher` trait.

Bitcoin-S now fully supports PSBTs with functionality for creation, updating, combining, signing, finalizing, and transaction extraction. You can find the documentation here: https://bitcoin-s.org/docs/core/psbts

#### Commits
277d62591f Bouncy Castle Fallback (#1232)
5e7cc6f980 Fix missing pattern patch case for when we have zero headers (#1242)
674ba24950 Added hash types to scriptsignature generators (#1219)
5ccd01ac77 Seq and Map Wrappers (#1131)
4ce05887f7 Update dns seeds to reflect what is currently in bitcoin core, some of the old seeds do not exist anymore (#1157)
c67629fd98 Reduce GCS generator params again to try and stop timing out on CI (#1144)
dad1fb746c Descriptor fixes and test (#1116)
5692a7da77 Replace null values with EmptyTransaction and PSBT.empty
8f70996f65 LnInvoice must ignore unknown LN tag fields (#1068)
d84c926aca Fix the order of LN tag fields in serialized invoces (#1066)
d858df743d PSBT Support (#1031)
8941ea02cc Bech32 weakness test vectors (#1056)
c4ade3ba3c Txo state flyway (#1052)
fcaa96665c Added PubKey addition functionality (#1051)
7512ca35d1 Updated P2PKWithTimeout to use CSV instead of CLTV (#1050)
3ffd9888ed Single Signing (#1020)
d75f75c0e7 Fixed BitcoinTxBuilderTest which was broken in #900 (#1021)
7ba1865d0a Increase core test coverage
82ef36e2a6 Create MaskedToString, implement it in ECPrivateKey, ExtPrivateKey, M… (#1011)
c738bb3b6d Support for payment secret and features LN invoice tags (#1012)
6d0bceb833 P2SH Signing (#993)
34a3efdcf9 Change ScriptPubKey to RawScriptPubKey in ScriptWitness (#975)
f280103e47 2019 12 17 sign ext key (#959)
d1007b4907 Fixed nLockTime setting when spending a P2PKWithTimeoutSpendingInfo (#973)
74f7f73797 P2PKWithTimeoutSPK (#967)
9916783863 Remove Sign trait from ECPublicKey, move signing functionality into ECPrivateKey (#962)
caf9b7d22c Add invariant to WitnessTransaction that says inputs.length == witnes… (#954)
2f2870013f Implement CurrencyUnit as Numeric (#932)
69077f8d08 Add little endian functionality to Network Element and Factory (#931)
971de61004 BIP32Path Factory and new tests (#928)
7cfd33b753 ChainQueryApi (#926)
d2bdf273fc Timelock bug fix (#920)
3183f17ce2 BitcoinTxBuilderTest Hang (#901)
c7a8a8eadf Add descriptors as toString for script pubkeys (#902)
b89d20e9a8 Moved BitcoinTxBuilder property tests into ScalaTest context, fixed bug where opPushData was marking valid short P2SH scripts as invalid (#900)
15ec7b89b6 Nicer Satoshis.apply (#899)
804f18f47f Refactor compact filter matching (#838)
306699ba8a MultiSignatureWithTimeoutScriptPubKey (#867)
7fe6604b26 Made ScriptPubKey and ScriptSignature toStrings nicer (#859)
254828e94e OP_NOTIF ScriptPubKey and Signing (#858)
4355543709 Conditional Signing Tests (#865)
3c7fd6c34a Nested OP_IF ScriptPubKey and signing (#857)
d86acfffed ScriptInterpreter Conditional Refactor (#855)
38db570e8f OP_IF Signing (#802)
3ed4c4aa84 RawScriptPubKey (#843)
20c42795b8 Replace scriptPubKeyToSatisfy with spendingInfoToSatisfy in Signer (#842)
ec38acbeb8 GCSTest fix (#845)
310ccbb838 Spending Info ADT use (#840)
5454d675f7 Signer UTXOSpendingInfo refactor (#830)
8dc005a586 UTXOSpendingInfo ADT (#834)
4641e0043c Created LockTimeSigner and delegated LockTimeScriptSignature signing from other places to it (#798)
d2b6a836c1 P2WSH Signer! (#797)
eb78d61c58 Removed the ScriptProgram companion object
8184f4ace1 Update script_tests.json, fix bugs that were unveiled with that updat… (#799)
e9c4b10f1c Script Program apply method refactor part 5 (#760) (#801)
cd76bf5455 Script Program apply method refactor part 7 (#760) (#804)
6d1b3049b5 Script Program apply method refactor part 6 (#760) (#802)
3ef128f4b3 Script Program apply method refactor part 4 (#760) (#800)
672446b7a5 Script Program apply method refactor part 3 (#760) (#795)
25f483104a Script Program apply method refactor part 2 (#760) (#794)
bbe1e87d94 Script Program apply method refactor part 1 (#760) (#793)
7b3e9f747a 2019 10 01 script program cleanup (#791)
c3df666aa5 2019 10 01 script interpreter cleanup (#772)
09ea1fb3cd Use the fact that network headers specify the number of bytes in the … (#783)
cee1e82d0c Reduce GCSTest by more, remove unused warnings when using scala console from sbt (#779)
4ee36e84e9 Removed the sealed trait and private case class Impl pattern from ScriptPrograms (#759)
933f0fcfd0 Initial BIP157 support (#695)

### Chain

#### Compact filters

We incorporated storage for `CompactFilterHeader`s and `CompactFilter`s in the chain project. For more information on what these are please read [BIP158](https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki). We currently cache all filters locally so that they can be reused for things like wallet rescans.

If you would like to see how wallet rescans work with the filters, please see https://bitcoin-s.org/docs/wallet/wallet-rescan

#### Commits
6d6b1023c5 2020 03 08 filter sync (#1209)
800fdffffb 2020 03 15 chainhandler getnancestor (#1247)
ba91c6107c Create removed Neutrino tables (#796)
3f734a95db Make ChainApi to scan the block filters in order to find matches (#786)
5cc0b30544 Optimize org.bitcoins.chain.blockchain.BaseBlockChain (#781)

### Cli

The CLI project has added support for PSBTs, getting the number of filters and filter headers, and doing a wallet rescan.

As well as new CLI commands, a Console CLI has been added. This allows you to access CLI commands from within the code base.

#### Commits
a043d3858e Remove requirement for sats parameter (#1190)
980d532b22  Allow getbalance to return in sats
c363156d8b Add codehause dependecy as a work around for issue 1110, there is a i… (#1105)
61dfa35d1e Console CLI (#1095)
a13feef57a Simplify txReads
5f1716b630 CLI Commands for PSBTs
c968e79c80 CLI command for filter and filter header counts (#1063)
7e6f489fee 2019 11 28 cli native image doc (#903)
43ec85af39 Rescan RPC (#854)


### Db commons

These are mostly minor bug fixes inside of this library. As db-commons is mainly used as a utility library for us for interacting with our application, most of these fixes do not apply to you. This package will more than likely be renamed to `app-commons` in the future.

#### Commits
a6e21fe43b Multiple Logger Fix (#1086)
ea555c5705 If migrations fail, attempt to baseline the database and apply migrations again (#1058)
f263f5c2a0 Add build config to skip publishing the new db-commons-test library (#1057)
81dcdbeb57 2019 09 27 logging refactor pt2 (#765)
dadd522c6a Remove hard coded log level in test app config (#757)

### Eclair

Our eclair rpc project now supports eclair `v0.3.3`. Here is the official release from ACINQ themselves:

https://github.com/ACINQ/eclair/releases/tag/v0.3.3

Here is there API documentation:

https://acinq.github.io/eclair/#introduction

For more information on how eclair works please see

https://bitcoin-s.org/docs/rpc/rpc-eclair

#### Commits

e387d075b0 Make eclair tests use bitcoind v19 rather than bitcoind v17 (#1187)
d4f3e184ef Use java.time.Instant to represent timestamps in EclairApi (#1118)
00feee8501 Support for Eclair 0.3.3 (#1097)
b83884e6f8 Eclair Web Socket client (#1006)
e1acac05eb Eclair performance tests
2527354bb8 Add comments indicating what time unit eclair sends things in for the… (#884)
cbf77119e7 Fix runnable not being cancelled when payment fails (#869)
73c734ff24 Fix monitorInvoice unit test (#846)
be8676e198 Fix getsentinfo Eclair Rpc call (#851)
2038faf126 Upgrade to ecalir v0.3.2 (#818)
6df467d295 Improve EclairRpcClientTest execution time (#826)
5ec86aef61 Fix bitcoind version for eclair tests (#778)

### Key Manager

The key manager is a new module this release. The key manager module's goal is to encapsulate all private key interactions with the wallet project. For more information please see: https://bitcoin-s.org/docs/key-manager/key-manager

#### Limitations

There is also an ongoing discussion about the two possible passwords the key manager can have:

1. BIP39 Password
2. A password used to encrypt the key material on disk.

https://github.com/bitcoin-s/bitcoin-s/issues/972

Currently bip39 password is supported at the library level, but is not supported for end users using the server project. [You can see that the bip39 password is hard coded to `None` here](https://github.com/bitcoin-s/bitcoin-s/blob/e387d075b0ff2e0a0fec15788fcb48e4ddc4d9d5/app/server/src/main/scala/org/bitcoins/server/Main.scala#L53).

There is a password that is used to encrypt your mnemonic seed on disk, but that password is hard coded to a default value. *THIS MEANS THAT YOUR MNEMONIC SEED CAN TRIVIALLY BE STOLEN IF AN ATTACKER CAN ACCESS YOUR HARD DRIVE. TAKE PROPER OPSEC PRECAUTIONS.*

Overall the key manager module should be considered insecure. For this release, it is more about setting up the API for further development in subsequent releases.

#### Commits

039722aedc Implement abililty to use BIP39 password. This means this password ne… (#990)
25916ac6f2 This creates a subtype BIP39KeyManager and moves all existing KeyMana… (#988)
8fb1716b1c Move initialization of wallet entropy into the key manager (#966)
72097e31bd 2019 11 30 key manager (#904)

### Node

We now support a neutrino node that can be hooked up to the rest of bitcoin-s to interact with the p2p network. Since neutrino is not officially released yet in bitcoin core, we have built a custom binary to run it. If you wish to experiment with neutrino please the website:

#### Commits

7fbc6423a5 Fix sync issues (#1090)
abaa0581c0 Rescan and fetch blocks (#835)
80c8636308 Disable OSX neurtino tests in CI (#777)
6476e3405d Add a log at INFO level to indicate we are making progress while syncing (#780)
a42d297eeb Remove bitcoind dependency from node (#770)
e66bf4c2e0 Remove isInitialized() check in sendMsg, which was causing a deadlock (#763)
71a136b03d Fix bug where we were sending messages before we were fully initialized (#755)

### Server

The server project is a bundle of all of our sub projects that you can send http requests to and have bitcoin-s do something. The easiest way to do this is with the CLI tool. The biggest change in this release is now we support configuration with a `--datadir` flag to specify where the bitcoin-s datadir rather than just reading the default. For more information read https://bitcoin-s.org/docs/applications/server

#### Commits

2896fd9c66 2020 02 21 datadir configurable (#1156)
30f5850860 Added server handling for empty account tables while a seed exists (#1094)

### Testkit

This release is enhancing pre-existing test case functionality and cleaning up bugs. We now have common traits for a lot of our older test cases that allow us to make it easier to refactor the entire code base. It also fixes a few small bugs in the fixture creation and deletion code that were the source of CI failures.

#### Commits
ba2cdedb4b Address issue 916. In our chain project fixtures we did not make sure that tables were fully created before trying to insert information into tables in futures. This causes race conditions on slow CI machines that are fixed now because we call 'makeChainHandler()' inside of the setupTableF flatMap (#1129)
7045fdb099 Add peers section to the example configuration (#1065)
62f2b19515 Refactor make dependent fixtures to use built in scalatest helper met… (#939)
089bb2bd72 Refactor old test cases to use BitcoinSUnitTest (#814)
27560ac1a5 Make tests to not require pre-installed bitcoind (#766)
029b106cd2 2019 09 28 common test trait (#767)

### Wallet

The wallet project now computes transaction confirmations dynamically, allows for more expressive states for transaction state (ie pending confirmation vs confirmed), as well as adds the ability for wallet recovery and rescans.

#### Commits
231aa3b519 (benthecarman/txo-state-test) TxoState Life Cycle Tests
9858718f19 Fix Wallet tests' keymanager to use changed config (#1173)
c7f8ab72cf 2020 03 06 wallet rescan test (#1218)
24fcf8c0c1 Reserved TxoState (#1111)
b50f818519 Create migration to drop confirmations column from txo_spending_info … (#1099)
bc9a25b47f 2020 1 11 accounts (#1022)
bf3a89bfdd 2020 1 04 fund raw tx (#1010)
1ad402ea56 Add more expressive txo states for the wallet (#1001)
d776e1c952 Automated wallet recovery (#985)
a31066d17e Move rescan logic from node to wallet (#974)
b92fc1cb8f Compute confirmations dynamically (#938)
a752c5301f Neutrino Wallet: OnCompactFilter handler (#905)
9e677b602d [Neutrino] Update balances (#888)

### Website

e3cbfdad5c ChainQueryApi doc (#1204)
3bfabe7bd5  Adding new SPK doc (#1208)
89f1db6ca7 Make imports in chain.md invisible (#1213)
d9881dd153 Fix typo in docs (#1203)
963752cbd2 Update eclair.md, also add code example (#1192)
267d5eb138 Update website configuraiton.md with migrations information (#1189)
a5ac2d8262 Add PSBTs to Docs Sidebar (#1172)
a3150d26a9 Added a getting-setup markdown doc for the website (#1167)
af9d11092f Update try-bitcoin-s.sh (#1165)
c026c9a7d8 Add section on how to generate a new version
fd8ec188b5 Create v0.3.0 of the website
67a4955bad  Add trusted peer caveat in node.md (#1250)
f9ef6e5a89 Add a philsophy section on getting-started.md, add missing libraries (#1251)
7f54b0954b 2020 03 17 secp256k1jni md (#1248)
677ec52a2d Bump docusaraus dependency to the latest version 1.14.4 (#1243)
dfeebf69e2 Add headings for both Node and cofiguration on the sidebar (#1241)
5e4d5fb002 Add testkit md (#1234)
87c768e0cc Add disclaimers to the wallet/key-manager around API breaking changes… (#1237)
e598d4c37e 2020 03 13 node md (#1229)
69783f9ed8 Order website sidebar by project, fix capitalization (#1226)
47665a28dc Remove unused imports in docs
6b5b70c2f8 Fix error, add await to psbt doc (#1220)
81ad5bb322 PSBT signing with BitcoinUTXOSpendingInfoSingle
190f49aad7 NodeApi doc (#1205)
af9d11092f Update try-bitcoin-s.sh (#1165)
fa0e0d1470 Add section to contributing.md for when travis ci won't run for new contributors (#1135)
a8a17d8793 Example config on in documentation (#1069)
af4744dcd9 Add server to sidebar.json so that it shows up (#981)
d78bcaaae3 Docs: update Security.md and fix edit button (#965)
31f10735a6 Update bitcoind docs to have correct versions
04b4aa49ea Update website following https://bitcoin-s.org/docs/contributing-website and running 'yarn upgrade' (#943)
fd64c42b69 Add a reference to ChainQueryApi to the wallet (#936)
0f9c349d51 Add documentation on env variable SBT_OPTS and add documentation about it (#914)
a91cff0d07 quick fix of link to txbuilder examples in docs (#815)
5d28e9143d Add akka logging documentation to our contributing.md (#764)


### Verifying signatures

This release is signed with christewart's signing key with fingerprint `339A49229576050819083EB3F99724872F822910`

You can find the key here: https://api.github.com/users/christewart/gpg_keys

Thanks to our contributors

- Ben Carman
- Chris Stewart
- cwaldron97
- dependabot[bot]
- Nadav Kohen
- philbertw4
- rorp
- Scala Steward
