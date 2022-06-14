# 1.9.2

This release is backwards compatible with the 1.9.2 release of bitcoin-s

See the individual module sections for more information on lower level updates to the codebase.

Want to get started quickly? See our `docker-compose.yml` file. [See instructions here](https://github.com/bitcoin-s/bitcoin-s/#docker)

If you are a typescript developer, [you can access the backend via our typescript library](https://github.com/bitcoin-s/bitcoin-s-ts)

## Running bitcoin-s

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-1.9.2.zip` and then run it with `chmod +x ./bin/bitcoin-s-server && ./bin/bitcoin-s-server` to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-1.9.2.zip` folder and start using the `bitcoin-s-cli` like this:

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
bitcoin-s_1.9.1-1_amd64.deb: OK
bitcoin-s-1.9.1.dmg: OK
bitcoin-s-bundle.msi: OK
bitcoin-s-cli-x86_64-apple-darwin: OK
bitcoin-s-cli-x86_64-pc-linux: OK
bitcoin-s-server-1.9.1.zip: OK

```

### Website

https://bitcoin-s.org/

### Releases

https://repo1.maven.org/maven2/org/bitcoin-s/

### Snapshot releases

https://oss.sonatype.org/content/repositories/snapshots/org/bitcoin-s/

# Executive Summary

## app commons

Adds the ability to stream logs from a process (such as tor) to our `bitcoin-s.log` file.

488716d10a Add ProcessLogger to ProcessBuilder so that we capture logs from binaries like tor (#4327)

## App server

Improves logging capability, we will now cap log files at 100MB and roll over to a new one.
The old file will be archived and compressed. The total archive size for log files is capped at 5GB.

This release also improves startup time of the backend by decoupling binding of the http server & tor startup.\

Finally, in some cases on raspberry pi's bitcoind can drop blocks and need to re-sync.
This release implements logic to retry connecting to bitcoind with an exponential backoff.

1ad540703c Improve bitcoind connection retry logic (#4386)
8a01432db4 Try to debug why shutdown isn't working on windows (#4349)
d335cd1933 Enable LauncherJarPlugin on oracleServer/appServer (#4338)
5036b37729 2022 05 11 tor race condition (#4333)
d46b4a6c91 Make logging to file async, remove neverBlock configuration so that we are guaranteed to capture logs (#4305)
be34593e80 2022 04 29 issue 4302 (#4304)
f4d864fab8 2022 04 26 Startup time of `appServer` (#4294)
17944c4aad Update rolling policies for log files (#4291)
bac3cb4190 Fix rolling log file (#4288)
16c5d835de 2022 04 20 issue 4280 (#4282)
e3e59923c4 Parallelize some startup on startBitcoinSBackend() to increase performance (#4217)

## bitcoind rpc

## bundle

The bundle will be removed in future releases. 
We are implementing a new electron GUI, it can be found here: https://github.com/bitcoin-s/bitcoin-s-ts

## Build

The major improvement this release is integrating [jlink](https://docs.oracle.com/en/java/javase/11/tools/jlink.html)
`jlink` allows you to ship with a custom JRE in the application you ship so the user doesn't need to download Java.

There is some caveats to using `jlink` on arm64 machines, see #4383.

cbeae5cdbc Remove carve out for protoc on aarch64 now that the akka grpc plugin has been updated (#4384)
171001273c Implement a workaround for 4369 on docker images and m1 macs IF the user has another java installed on their machine (#4377)
0a0fc92f32 Switch base docker image to ubuntu to get docker working again (#4367)
e4d38ba53a 2022 05 05 OS specific jlink builds (#4322)
dd9a9dcea6 Remove explicit inclusion of jdk.unsupported as its not included by default (#4319)
087b9f90b5 2022 05 03 oracle server jlink build (#4316)
cbfe684352 Reduce what gets tested on tor CI (#4274)

## Core

Begins laying the ground work for Taproot by including the `XOnlyPubKey` data structure.

Adds various TLVs defined in BOLT14.

Finally fixes various bugs and improves ergonomics of the core library.

ddbdde495d 2022 06 13 taprootspk xonlypubkey (#4388)
7e2ecd9d6a Added data structure for x-only public keys with undetermined parity (#4387)
5f82307e27 Added Compute Contract Id test Vectors (#4385)
344a8fd759 Add TLVs defined in BOLT 4 (#4380)
b021649ac4 Refactor WitnessVersion.rebuild() to be Either[ScriptError,ScriptPubKey] to make the taproot implemtation easier (#4382)
ab215e26df Set recovery id properly for buildLnInvoiceSignature (#4379)
efc1f9fb77 Have Satoshis extend Numeric (#4364)
676c0b4261 Add isStandard to Address (#4353)
90970058f9 Improve TLV error message (#4283)

## Crypto

Rework how `HashType` is handled in the `ECDigitalSignature` API.
Previously it was extremely confusing whether an `ECDigitalSiganture` had a `HashType` or not.
Now you can call `ECDigitalSignature.hashTypeOpt` to determine if the signature has a `HashType`.

b80bf4649e Add HashType to ECDigitalSignature API (#4320)
f42d7ae8e7 Added validation to signature methods to avoid corruption edge cases (#4214)

## db commons

Allow other library users to access the internal flyway configuration.

441937238f Make flyway protected so other apps can access it (#4372)

## DLC node

272f31aeaa Fix race condition on DLC node startup wrt to tor (#4335)

## DLC wallet

This release of the DLC wallet introduces the concept of `Contacts`. 
These are people you are frequently doing DLCs so we should save their information to make it easier to enter
into a new DLC with that same contact.

This PR also adds a mapping between DLCs in the wallet and the contact you are doing the DLC with.
Now the contact information will be shown by the DLC so you can remember who you did the DLC with.

fdf281b469 DLC <-> contact mapping (#4346)
79b4f096ec Improve logging around signDLC (#4299)
525fb2ac0d Default createDLCOffer to current block height (#4285)
f5940c93d4 Contact list (#4234)
d29bad3437 Add better logs for a DLCWallet.cancelDLC() (#4278)

## gui

## keymanager

## Lnd rpc

Update lnd to 0.14.3, implement probing with lnd, and various bug fixes.

762202a54d Add test/example on how to use the channel acceptor (#4375)
c2d8735dd7 LND: Add configs for gossip in test env (#4378)
45777f2bb0 Fix outPointToChannelPoint to use correct endianness (#4376)
54c3f77f8e Improve lnd test suite reliability (#4361)
527e3ae862 Fix lnd sendToRoute for 0 amount invoices (#4348)
e9582d2145 Update lnd to v0.14.3-beta (#4347)
63e8d76dfc Add ability to get LndInstanceRemote from config (#4334)
6845caf778 Make all uint64 types from lnd a UInt64 (#4332)
18c5ded5d3 Check if lnd network config is equal to 1 (#4330)
8ff4ee13e5 Use route hints with probing (#4312)
16c13568a9 Lnd probing: only update route with mpp record if we can (#4293)
b8a984a986 Implement probing in lnd (#4202)
95bbb06789 Add lnd invoice client (#4289)
587bca87c4 Add router rpc to lnd, use for paying invoices (#4286)
5856745398 Add raw funding of psbt to LndRpc (#4235)

## Lnurl

Adds an initial implementation of the [LnURL specification](https://lnurl.com/). 

d60d984a6b LnURL Module (#4295)

## node

Removes old SPV code as that was not used and is being slowly deprecated on the bitcoin network.

d8fc8e588f Remove Spv code (#4356)
b980c432fd Bump node initialization timeout to 20 seconds (#4328)
ce00d3ac36 Segregate handling of Tcp.ErrorClosed command from the rest of Tcp.ConnectionClosed (#4307)

## Oracle Explorer Client

Adds tor endpoints for the Suredbits Oracle Explorer so you can send announcements/attestations directly to the hidden service.

c9502babba Tor endpoints for the oracle explorer client (#4314)

## wallet

This release fixes a bug where we could create a duplicate UTXO in the wallet.
If this bug is detected in the wallet, this will trigger a rescan on wallet startup to correct wallet state.

This also fixes a bug in our sql queries where we fetch too many outpoints at one time for a block.
This resulted in a SQL exception. Now we break the query up into smaller queries.

bf88d0d93f Remove exception when we have zero relevant outputs (#4352)
f680ab8691 Persist whether wallet is rescanning in the database (#4326)
059f2f5fac Fix `ERROR: relation "txo_spending_info" does not exist` (#4323)
341c712563 Validate bitcoin-s.wallet.walletName config option (#4336)
fac0713405 Reduce rescan threadpool size to just be number of available processors (#4306)
0c6c9180ed Handle duplicate UTXOs (#4296)
486fa36d2c Make _findOutputsBeingSpentQuery take at most 1,000 outpoints (#4300)
6db1f26625 2022 03 22 getrelevantoutputs upfront (#4219)
3831b35817 Prevent the wallet from creating duplicate UTXOs (#4290)

## testkit-core

## testkit

## tor

9d90b2279a Tor v0.4.6.10 (#4331)
6356a50a89 Add log for tor being provided (#4329)

## Website

4c74f54741 Made change to cli.md file, clarifed only need to do either (Building the command line interface) or (Native binaries) section (#4373)
1706ec8e84 Fix typo (#4365)
4fc3b05ed5 Upgrade website dependencies (#4363)
72fef5a27e initial draft of 1.9.2 release notes (#4362)
c911808996 Fix example (#4324)
67f8ac8294 Add cd bitcoin-s-server (#4298)
a739a2dd2b Bump README versions (#4284)
b0e849c233 Update pgp key as previous key expired (#4273)
8cda343fd3 Add version 1.9.1 to the website (#4269)

## Dependencies

2af7923f3b Downgrade slick to fix jlink build (#4345)
107f95cd5a Upgrade sttp to 3.6.1 (#4341)
63df47e002 Upgrade to slick to 3.4.0-M1 (#4342)
4b2ca33495 upgrade micropickle to 1.6.0 (#4340)
a3faa0c56f Upgrade plugin dependencies (#4318)
678dc6f676 Upgrade sbt native packager to 1.9.9 (#4317)
e05cf21827 Upgrade dropWizards to 4.2.9 (#4313)
2f4bbf7014 Upgrade flyway to 8.5.9 (#4311)
56138cea92 Remove source code dependency, its not used anywhere (#4310)
3dc709386a Upgrade Eclair to v0.7.0 (#4308)
7566a96b31 Update akka to 2.6.19 (#4287)
813b58e977 Update deps (#4279)
ca5bde46d4 Bump prismjs from 1.26.0 to 1.27.0 in /website (#4133)
47b65cae65 Bump async from 2.6.3 to 2.6.4 in /website (#4271)
