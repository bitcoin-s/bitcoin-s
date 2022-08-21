# 1.9.3

This release is backwards compatible with the 1.9.x series of bitcoin-s

See the individual module sections for more information on lower level updates to the codebase.

Want to get started quickly? See our `docker-compose.yml` file. [See instructions here](https://github.com/bitcoin-s/bitcoin-s/#docker)

If you are a typescript developer, [you can access the backend via our typescript library](https://github.com/bitcoin-s/bitcoin-s-ts)

# Executive Summary

- Many websocket events added
- `loadwallet` RPC to switch wallets without restarting the application
- taproot script/signature verification
- multi peer neutrino

## Running bitcoin-s

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-1.9.3.zip` and then run it with `chmod +x ./bin/bitcoin-s-server && ./bin/bitcoin-s-server` to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-1.9.3.zip` folder and start using the `bitcoin-s-cli` like this:

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
bitcoin-s_1.9.3-1_amd64.deb: OK
bitcoin-s-1.9.3.dmg: OK
bitcoin-s-bundle.msi: OK
bitcoin-s-cli-x86_64-apple-darwin: OK
bitcoin-s-cli-x86_64-pc-linux: OK
bitcoin-s-server-1.9.3.zip: OK

```

### Website

https://bitcoin-s.org/

### Releases

https://repo1.maven.org/maven2/org/bitcoin-s/

### Snapshot releases

https://oss.sonatype.org/content/repositories/snapshots/org/bitcoin-s/

## Contributors

```
    88	Chris Stewart
    38	benthecarman
    13	rorp
    10	Shreyansh
     4	GreyMcCarthy
     1	Nadav Kohen
     1	fiatjaf
     1	user411
```

# Modules 

## app commons

Consolidate duplicate data structures in `cli` and `appServer` into a common location.

825937ea5f Add more JSON serializers (#4628)
b7c7cb8fc2 Moving duplicate dataStructures in `cli` and `app-server` into `app-commons` (#4541)

## App server

This release adds the ability to call `loadwallet [wallet-name] [aes-password-opt] [bip39-password-opt]`.
This RPC allows you to load a new wallet in bitcoin-s without shutting down and restarting the server.

This release also adds the rpc `exportseed [wallet-name] [aes-password-opt]` to export the mnemonic seed for a wallet.

Websocket events added this release are 

- fee rate change
- rescan complete
- sync complete
- tor started

b18da7ac2b Fix bug where it prevented us from rescanning an empty wallet (#4632)
153d20128f Improve logging to try and solve 4618 (#4622)
85c01f7a8c More lazy initialization for BitcoinSServerMain (#4621)
1b19872ac4 Implement `exportstakingaddresswif` (#4617)
b6710e7043 .map() on loadwallet result before starting http server so wallet is fully loaded (#4614)
a0851fea58 Run migrations upfront (#4607)
d0dadfa423 2022 08 11 issue 4600 and only emit `syncing` ws event when we are actually syncing (#4604)
b0831f26a7 Refactor waiting for bitcoind lost blocks into helper method, add context (#4602)
95139bdc3d Improve logging on getBlockchainInfo() (#4590)
9f89ba9b7a `loadwallet` endpoint (#4417)
a02e25b0ce Refactor `WalletRoutes` to take `DLCWalletLoaderApi` as a paramete (#4565)
8cb45e8208 Add unit tests for loader rescans (#4570)
1d3db879ad Add rescanState variable to DLCWalletLoaderApi (#4569)
ec0103dab3 2022 08 02 loadwallet startup logic (#4568)
191df09196 Move pollBitcoind out of startBitcoindBlockPolling (#4559)
936a65edd4 2022 07 21 Modify helper methods in `BitcoindRpcBackendUtil` to not materialize streams eagerly. (#4526)
a2e7c428cf Bump sleeps on CallbackUtilTest (#4534)
d508d65142 Implement ability to cancel bitcoind polling jobs (#4538)
4ef27dd163 Fix bug where dlcNode was not apart of tor dependent configs (#4537)
67dd16d8ad Add missing calls to BitcoinSAppConfig.stop() (#4535)
d4210fad65 Pull over changes from loadwallet branch (#4527)
5d309fbae0 Fee rate WS notification (#4518)
e3a7c0971f Refactor to hide the Message paramter that gets fed to the websocket queue, move it to a internal implementation detail (#4514)
f6df30c45e Make appServerTest run in parallel to speed up test suite (#4515)
cdef8c01dd Set syncing flag to true on startup (#4506)
d1375a589e 2022 07 12 server startup cleanup (#4501)
e90e372e54 Add `sync` JSON field to the `getinfo` endpoint output (#4452)
84ce76e16c Fix race condition related to sqlite3 database not being created before we access it (#4479)
20bf1e6a08 Remove map on tor to allow access to getdlchostaddress endpoint before tor is fully stated (#4435)
a7aad46934 Seed backup API (#4357)

## bitcoind rpc

This release adds official support for v22 and v23 of bitcoin core.
It also includes some basic support for fetching blocks via akka streams
and taproot.

ea123139d9 Remove support for bitcoind v16 rpc client (#4634)
d95d3db75c Download `arm64-apple-darwin` bitcoind binary on M1 Mac OS X (#4588)
7b754138b8 Drop support for the experimental bitcoind version (#4586)
ed4e332cef Dafalut to the latest Bitcoin Core version (#4579)
ad21a11254 Create BitcoindStreamUtil and refactor to use it (#4578)
3ae169a41f V22 RPC Support Update Continued (#4424)
99b75d166f Fixes support for bitcoind taproot descriptor wallets (#4415)
4dc1bc7050 add bitcoind v23 rpc (#4368)


## bundle

## Build


Implements electron app builds to combine bitcoin-s backend and bitcoin-s-ts
frontend into a single electron app.

Adds a flag `DISABLE_JLINK` to disable jlink when running the artifact built by `universal:packageBin`.

This release fixes a package name bug introduced in 1.9.2 (#4401).

Fixes various bugs in bash scripts used by `bitcoin-s-{server, oracle-server}`.

28311e1550 Only build native bitcoin-s-cli on tags or merges to master (#4636)
713ee75d4c Get rid of standalone scalajs, mdoc CI matrix. Consolidate with other CI jobs (#4633)
2dad9f57b8 Fix bug where we weren't specify the bash script for each project to modify file permissions (#4625)
ac7939ac54 Adjust bash script permissions so they can be run by any user (#4624)
6b8f45e393 Update docker-compose.yml (#4599)
e413f04106 Remove the daemonUser/daemonUserId combination to make our docker images more portable (#4601)
a2117e2551 Downgrade CI jdk 18 -> 17 (#4546)
6f42f83146 Move OSX check inside of jre path check, fix bug where quarantine wasn't working in case where we are in the same directory as script (#4508)
5b44c074f1 Fix `chmod: jre/bin/java: No such file or directory` errors (#4493)
de935032ea Make coreTestJVM.dependOn(testkit), add new multi core dispatcher to allow more cores for TaprootTTests to hopefully take load off the scala execution context (#4477)
9985db3b41 Bump to jdk18 (#4471)
3ea4763cfc Change 2.13.7 -> 2.13.8 in a few spots where I missed it (#4470)
2968a9ee15 Try to test out setup-java CI action (#4466)
76694a55e9 Add server build matrix, mac,linux electron builds (#4434)
aed92c35af Implement DISABLE_JLINK env variable to disable jlink jre's usage at runtime (#4426)
e25c24dc9b Fix package name bug so package name is  ra… (#4402)

## chain 

27120a2bb9 Add InvalidBlockHeader ChainException (#4635)

## Core

This release adds support for the [taproot script interpreter](https://github.com/bitcoin/bips/blob/master/bip-0342.mediawiki)
and [taproot signature serialization](https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki)

The next release will add signing support for taproot transactions.

c34b0de886 removed logger parameter from where it's used with callbacks (#4598)
26492d2449 Add descriptor for TaprootScriptPubKey (#4595)
b04a34ad02 Use FutureUtil.makeAsync where we are attmepting to create async Futures (#4583)
4b83286922 2022 07 25 wallet api refactor (#4545)
c210052640 Refactor coin selection to be not be bitcoin-s specific (#4496)
5c9092889b Add abillity to clear callbacks (#4512)
9412170f09 2022 07 11 fix invalid csa neg (#4495)
f8f4facdd1 Correctly parse TaprootKeyPath how they are represented (#4494)
b16a8ca6aa Fix bug where we weren't checking if a TaprootKeyPath w/ annex has two stack elements (#4491)
59732809d0 Represent and handle `SIGHASH_DEFAULT` correctly in `TaprootKeyPath` (#4488)
ef50becf1b Enforce MAX_PUSH_SIZE for taproot (#4486)
db63b286b3 Fix hashtype check to correctly handle scala's conversion (#4487)
91a1c84a6f Enforce minimalif for tapscripts (#4484)
990e344a49 Disallow OP_CMS in taproot (#4485)
cc45924ed8 Validate max ScriptNumber bytes in OP_CHECKSIGADD (#4483)
3892fa23ef Restrict hash types allowed by taproot signature verification (#4481)
3821060e68 Schnorr sig parsing checks (#4482)
ec599a5c3d Fix `containsOpSuccess` implementation (#4480)
0397ca6c78 tapscript/sigopsratio_3 failing correctly (#4461)
5df5bf6741 Add support for taproot keyspend PSBTs (#4462)
000e7a7930 Have FutureUtil.makeAsync handle thrown exceptions (#4458)
211339f344 Add static test vectors for Taproot (#3769)
83cff9a44c 2022 07 07 issue 4455 (#4457)
97be26586 Get a SIGHASH_SINGLE test case working (#4431)
eae16a52f8 Allow creating input info of simple OP_TRUE script pub key (#4450)
11f6c8f024 2022 07 05 `UnknownControlBlock` (#4449)
6f6315c1e7 Fix taproot SIGHASH_SINGLE taproot annex hash bug (#4448)
64183568fe Allow creation of TaprootTxSigComponent (#4445)
5f93096c5a Fix Taproot keyspend serialization (#4443)
93302fea01 Fix TaprootKeyPath.isValid for 64 byte sigs (#4444)
0c14fc961b Add test case for `SIGHASH_ALL_ANYONECANPAY` in taproot (#4442)
3122e1d0f8 Fix bug in taproot SIGHASH_ANYONECANPAY_SINGLE impl (#4440)
38d8f8cdf0 2022 06 30 `OP_CODESEPARATOR` impl compatible with taproot (#4439)
a97be26586 Get a SIGHASH_SINGLE test case working (#4431)
c4fd7035be Add Taproot PSBT fields (#4420)
124cbe4b67 Add annex to `TaprootKeyPath` (#4416)
5115d8d076 Move lastCodeSeparator to StartedScriptProgram (#4414)
a680f03c04 Implement BIP341 (#4409)

## Crypto

This release implements [musig2](https://github.com/jonasnick/bips/blob/musig2/bip-musig2.mediawiki)

2b60bbb1c3 Remove .map() and use .foreach() with buffer in CryptoBytesUtil.toByteVector() (#4454)
ae0962d7ed Musig2 Implementation (#4418)

## db commons

675b210333 Add PSBT database mapper (#4584)

## DLC node

## DLC wallet

Optimizations for the DLC Wallet.

34c10c6c65 Fix WalletDLCSetupTest (#4623)
46502496c1  Update DLCClientIntegrationTest to use the newest bitcoind (#4596)
a22e847e2e Implement small optimization to not query for DLCDb twice (#4592)
326cb9845e Implement listDLCs(state), use it in getWalletAccounting() (#4591)
ba396f2fb3 Fetch findByFundingTxId/findByFundingOutPoints in parallel as an optimization (#4560)
603b7e0aea Make DLCWallet.listDLCs use DBIOActions (#4555)

## gui

## fee rate

This release fixes a bug which can cause an application to hang for a long time
when requesting a fee rate. Now we have a timeout of 1 second when fetching fee rates.

6dfa7683e4 Implement a timeout on fee rate requests (#4503)

## keymanager

This release of the keymanager now keeps track if seeds have been imported
into bitcoin-s or generated from internal entropy.

08699c0ee0 Remove nested Option[Option[AesPassword]] (#4564)
948d2b424e Make KeyManagerAppConfig.walletName return String rather than Option[String] (#4507)
414557effb Add `imported` flag to the seed JSON (#4459)

## Lnd rpc

64bc1367c5 Add listPendingChannels to LND (#4603)
ef3bfed1dc Add ChainNotifier to LND (#4589)
f286b42c71 Retry lnd startup if it fails (#4573)
678612161b Fix lnd OutputDetails for outputs that dont have an address (#4438)
828d03c727 Fix out of bounds issues with lnd mappers (#4425)
0b9190f968 Lnd v0.15.0-beta (#4419)
531845adfa Accept keysend and amp in lnd fixtures (#4407)
9b1f8924ab Fix download lnd command (#4406)
04802ae239 Add peers rpc to lnd (#4403)
a0ab0638f8 Allow for custom channel sizes in LndRpcTestUtil.createNodePair (#4398)

## Lnurl

## node

This release merges multi-peer neutrino support. This release does NOT enable multi peer support by default.

These new configurations are introduced in #4408. Adjust it if you want different behavior.
```
bitcoin-s.node.peers = [] # put ip/dns addresses here to force connections to these peers
bitcoin-s.node.maxConnectedPeers = 1 # try to connect to peers from dns seeds, database, addr messages etc
bitcoin-s.node.enable-peer-discovery = false # use the defauls suredbits neutrino node as a peer
bitcoin-s-.node.use-default-peers = true # whether to use suredbits hosted peer
```

This PR also implements `NodeCallbacks` with akka streams so we can safely call `loadwallet`
when node callbacks are being executed. Now we want for the streams to complete, and then continue
loading the ne wallet.

d241e6f9e4 Remove merklebuffers (#4615)
dea99457b5 fix mac node-wallet test failure (#4585)
5acbba9377 Replace `BoundedSourceQueueWithComplete` with `SourceQueueWithComplete` (#4576)
c4d358061a Add P2PClientSupervisor (#4509)
f5cca7e5e1 Add guard for calling NodeCallbackStreamManager.stop() (#4523)
0a127368f0 2022 07 18 node callback stream manager (#4520)
2e309086d5 Fix block fetch issue #4513 (#4522)
0cb93ddbf4 2022 07 17 node callback akka streams (#4516)
0b53c7be12 Change tests to use disconnectnode rpc instead of restarting bitcoind (#4468)
7c649d39db Fix filter sync if headers received while syncing (#4463)
111df25df0 Fix sync issues post #4408 (#4441)
42564bc810 Find and switch peers (#4408)
a7ba46f67d Update hardcoded seeds (#4412)
7f43ef98ad Default to suredbits node if peers field is left empty (#4404)

## Oracle Explorer Client

8638d14ca0 Update Oracle Explorer addreess (#4609)

## wallet

This release introduces akka streams in the wallet module.

With this new dependency, we implemented rescan's with streams so that the rescan can be arbitrarily terminated
by the user. The streaming feature was needed to implement the `loadwallet` rpc call safely when a rescan is ongoing.

The wallet now emits a websocket event when a rescan is complete.

This release also fixes a bug in rescans where a single tranaction pays to multiple wallet addresses.
If those addresses were generated in _separate_ batches during a rescan, funds wouldn't be discovered.
This is unlikely to happen in practice. Consider rescanning your wallet though on the 1.9.3 release.

Various refactors and optimizations were needed to complete the rescan and `loadwallet` work.

eb1327824b Fix bug where we were not generating change addresses during a rescan (#4631)
6119a334fa Make fundRawTransactionInternal use DBIOActions (#4575)
cf22816003 Fix rescan batch boundary bug (#4549)
2fa7c39f64 Use DBIOActions to speed up processing transactions (#4572)
c03b158f94 Implement `RescanTerminatedEarly` exception to terminate the stream, implement `ArgumentSource` (#4574)
524c0af536 Make wallet.getBalance more effienct (#4566)
78648f1e40 PostgreSQL friendly wallet names (#4571)
c89491c3e7 2022 07 29 fix multiple batch test (#4556)
b1a7f92c67 Refactor to not use filterThenSum() when calculating balances (#4562)
af9e5eaeef Expose DBIOActions for various models in wallet, remove address queue (#4524)
17ef1dcdd9 Fix rescan flag getting set, add unit test (#4554)
f608c5d5e4 Create a helper case class called `FundRawTxHelper` (#4544)
b69e487d04 Move `MockChainQueryApi`/`MockNodeApi` out of `BaseWalletTest` (#4542)
6c76639ba3 Move processBlock into WalletApi (#4543)
44e09270d9 Move getNewAddress(account) to HDWalletApi (#4540)
b905afa65e 2022 07 20 wallet rescan stream (#4530)
1ae4d40dac Implement `rescancomplete` callback (#4475)

## testkit-core

Fixes regression that broke test logging.

aca59ca352 Fix test logging (#4446)

## testkit

Various PRs to improve flexibility and clean up resources allocated by tests.

8b646802b3 Fix postgres database not clearing after test (#4558)
f487c1270e Implement cleanup of dlcWallet/wallet DbAppConfig threadpools (#4557)
2cd8c80f67 Refactor so we don't create multiple WalletAppConfig in testkit (#4548)
f1e9a34690 Remove hard coded values for embedded pg config (#4533)
412e6a06c4 Allow pg embedded better support for 3rd party libraries (#4532)

## tor

Emit a `torstarted` websocket callback when tor is fully started.

1b169d8fa0 Implement `torstarted` websocket callback (#4476)

## Website

531fd0f865 Update 1.9.3 release notes through #4609 (#4612)
9fd9cf1ee4 Update release notes again for 1.9.3 (#4597)
c7e5317294  2022 08 06 1.9.3 release notes (#4582)
6b9d2db350 Update license to latest year (#4525)
135b9f8a35  Update documentation for UI to use build script Advanced Setup  (#4430)
305e920784 Remove extra stuff from README so its easier to find important stuff (#4433)
906e1f5c53 Bump previousStableVersion to 1.9.2 (#4432)
0f3c28e23e Fix UI build instructions (#4405)


## Dependencies

d79f70ee82 Bump scalatest to 3.2.13 (#4605)
c10f7beadb Run yarn upgrade to update website deps (#4581)
9930c964f7 sbt 1.7.1 (#4497)
1fc6d2a793 Upgrade sbt to 1.7.0 (#4490)
0404cffe26 Embedded Pg v1.0.1 (#4567)
970b6fd6eb upgrade scalatest dependencies (#4411)

