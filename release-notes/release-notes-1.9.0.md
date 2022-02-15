# 1.9.0

This release is backwards incompatible with previous releases of bitcoin-s.
You will not be able to build a DLC with prior releases of bitcoin-s. This was due to a bug
in how we computed `contractId` in a DLC. See #4012 for more information.

See the individual module sections for more information on lower level updates to the codebase.

## Running Bitcoin-S

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-1.9.0.zip` and then run it with `./bin/bitcoin-s-server` to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-1.9.0.zip` folder and start using the `bitcoin-s-cli` like this:

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
$ sha256sum bitcoin-s-server-1.9.0.zip
aa1084edb5fcd3d1dbcafe0d0fba787abf4cd455bbe38809bd9a65a49c0cd0eb bitcoin-s-server-1.9.0.zip
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

### Snapshot releases

https://oss.sonatype.org/content/repositories/snapshots/org/bitcoin-s/

# Executive Summary

This release integrates Tor network support for opening a DLC with your counterparty.
This is a huge UX improvement over the previous flow where 2 _manual_ round trips
were required to open a DLC with your peer.

See individual module sections for updates on per module basis.


## app commons

Bug fixes and improve the API for `AppConfig`

4e6c68155e Sort outcomes in `decodeoffer` response (#4094)

142612f034 Make newConfigOfType use Vector[Config] (#4039)

2066447cdc Add isEndpoint to numeric contract descriptor serialization (#4033)

e802254a20 2022 01 24 rm appconfig varargs (#4011)

3162c1b2d0 Ignore .DS_Store files when making backups (#4000)

a7af8cac4c Patch bug where `zipdatadir` doesn't work if the directory was not created (#3959)

## App server

The major features shipped this release is support for websockets.
You can now be notified of websocket events from the wallet, dlc wallet, and chain.
These are used on the [wallet-server-ui](https://github.com/bitcoin-s/bitcoin-s-ts/tree/master/wallet-server-ui) to notify when events happen

This module will be renamed to `Wallet Server` in future releases as that
is a more accurate name for this module.

There are various PRs that broke API compatability from `1.8` -> `1.9`. See [the list](https://github.com/bitcoin-s/bitcoin-s/pulls?q=is%3Apr+label%3Aapi-breaking+is%3Aclosed+milestone%3A1.9)

08941206fc Bump timeout to 60 seconds (#4021)

d2f53db82e Change BTC RPC log level (#4010)

f438ce2897 Refactor zipDatadir (#3999)

d88e2494e0 Make RPC authentication optional for localhost (#3980)

ee3ee53191 RPC password authentication (#3961)

9dd126bb9f Modify estimatefee endpoint to return a number rather than a string (#3973)

66e23b61a8 2021 01 11 issue 3938 (#3971)

eeeecb00c5 Reduce polling log to trace when we try to poll bitcoind (#3964)

5de325e7de Exclude seeds from backup (#3950)

f8f8f50aae Implement ping every 5 seconds to keep websocket connection alive (#3947)

7527388be5 2021 12 20 ws dlc callbacks (#3926)

50bec2abc6 2021 12 14 websockets (#3906)

4b07629d56 Add `getmediantimepast` RPC call (#3921)

4646ef6e19 Remove BitcoinSServerMain.startedFP (#3928)

3cd57d37f5 Remove backupwallet / backup oracle (#3920)

f8f5a8e8dc Update docker-application.conf (#3905)

7e214da601 Improve `BitcoinSAppConfig.zipDatadir` (#3903)

6c67e83d3f 2021 12 10 issue 3891 (#3894)

41585a18ca Make Bitcoin Core RPC backend to retry on Error -28: Verifying blocks (#3896)

a38309bed1 Add range of block heights to the error message (#3886)

d393848cc2 2021 11 30 issue 3847 (#3862)

0b9500f4f2 Add `hex` fields to `decodecontractinfo` JSON response (#3830)

1d7529236f 2021 11 13 scan bitcoind witspkv1 mempool (#3825)

5189b6ad5c revert logging directives on request/response back to DEBUG (#3819)

a40ef1ab21 Break existing api to get new address to remove the requirement for null label (#3818)

21fab4ee70 Improve app server validation error handling (#3796)

403c78fd8f Server Docker config (#3775)

## bitcoind rpc

Implements rudimentary support for bitcoin core v22. This release does not implement all RPCs on v22.

5366428fb2 Implement `BitcoindRpcClient.epochSecondToBlockHeight` (#4068)

c0c54acc24 Make non-final bitcoind RPC error message more descriptive (#3915)

a6898defe2 Support for Bitcoin Core v22 (#3834)

## Build

Ergonomic improvements for runnign test suites, make the installer names more readable
for non technical users.

52dcf51e82 Automatically download binaries if they are used in test suite (#4005)

e24efd65ae 2022 01 12 docker ws (#3976)

6e7af37ca0 2021 12 03 installer rename (#3876)

cd3006c020 Verify binary download hashes (#3849)

## Cli

## chain

Adds a callback so that we can be notified when a header is processed by the chain module.

7ee1f0f406 Implement batching of database calls for our chain callback (#4003)

d06b064b6b 2021 12 28 blockprocessed callback (#3946)

## clightning rpc

This is a new module in the 1.9 release. It adds support for a clightning RPC.

cb704da927 Add clightning sendcustommsg (#3883)

7933c90741 Update c-lightning to v0.10.2 (#3857)

cd5451adaa Add clightning listtransactions func (#3797)

01a7c7c838 Make clightning tests execute async (#3768)

6d43d443ba Initial c-lightning rpc support (#3755)

## Core

This release merges support for general payout curves in the `core` module for the DLC specification.
See #3854 for more information

This release also merges support for disjoint union contracts in the DLC specification.

This release fixes a bug in our `contractId` computation.
This breaks backward compatibility in older releases of bitcoin-s.

13c46e0af8 Move expensive script checks to last inside of P2PKWithTimeoutScriptPubKey (#4087)

e2b9c458e4 Change `DLCUtil.buildOracleSignatures` (#4061)

7a6f0430d6 2022 02 03 issue 4032 (#4042)

71711ca582 Add invariant to make sure spendingTxId is different than the txid (#4019)

b918cf78b7 Fix bugs where we were building internally inconsistent SpendingInfoDb (#4016)

d983ad14f3 Fix contractid computation (#4012)

21de609ed8 2022 01 22 cetsignatures refactor (#4004)

214213b59d 2022 01 16 issue 3983 (#3987)

93c5121632 2021 01 06 tlv invariants (#3965)

8857af2b66 Address ben's code review on #3854 (#3962)

b342f373ae Make ContractDescriptorTemplate an unsealed trait (#3963)

8c5288d758 Implemented general payout curves (#3854)

6652448f99 Added constructor for p2pkh for decompressed public keys (#3944)

d66afe9e43 Add satoshisRounded to Bitcoins (#3904)

0d37c4b54f 2021 12 08 block parsing bug caused by 65 byte taproot signatures (#3887)

8765c2f845 Disjoint union dlc (#3839)

422aea2242 Fix TypeIdentifier fromBytes (#3832)

0b3654f020 Remove scientific notation from Bitcoins.toString() (#3811)

09c2562675 Added WitnessScriptPubKeyV1 for sending to Taproot addresses (#3737)

aa748c012f 2021 11 03 protocol version (#3793)

18726c10bb Move ValueIterator out of TLVFactory trait (#3794)

92cf042ccb Expose MnemonicCode.toEntropy() and MnemonicCode.toEntropyWithChecksum() methods (#3786)

92ab9faa45 Decide which coin selection solution to use based on waste metric (#3646)

## Crypto

f4a2ec8554 Make AesEncryptedData a network element / factory (#3952)

## Db commons

A major theme this release is enabling our APIs to use [database actions in Slick](https://scala-slick.org/doc/3.3.3/dbio.html).
This allows us to build database transactions to avoid corrupting data.

We also fixed some bugs in our database implementations around storing very large scriptpubkeys.

4d85b7a3d7 Support for big SPKs (#4084)

3d674c37f3 Create upsertAllAction (#4073)

2166faf61d Make findByPrimaryKeysAction public (#4079)

3991789129 Don't call findByPrimaryKeys if ids is empty (#4074)

48189d5c1d Make `CRUD.run` and `CRUD.runVec` transactional (#4059)

0079489a15 Create DbMapper for DoubleSha256Digest (#3995)

6c2bb0d111 Rename database username and password variables (#3930)

afb51228b4 Add NodeId to DbCommonsColumnMappers (#3880)

085b8b1910 Implement CRUDAction.deleteAction() (#3877)

2d5732375f Add CRUDAction.{updateAllAction, updateAction} (#3872)

f71d3567ed 2021 11 23 crud action (#3851)

## DLC node

The major change this release is implementing a connection timeout for dlc node.
If we cannot connect to a peer within 45 seconds, we throw a timeout exception.

55ecc1ae8f Implement connection timeout (#4081)

019c9b2644 Fix log message failure in DLCDataHandler (#3845)

41cb26a3bb Make DLC node's external IP configurable (#3838)

## DLC Oracle

Improvements to the database handling code so that announcement/attestation creation is transactional.

49d4d7f179 2022 02 06 issue 4049 (#4056)

7f344ad3ff 2022 02 06 announcement creation logging (#4055)

524c5212e0 Add MasterXPubTable to list of DLCOracle tables (#3955)

c3d1b2ee12 Give DLCOracle to filter events by pending/completed (#3953)

## DLC wallet

This release of dlc wallet fixes bugs for accepting offers multiple times.

We also add a new column in `global_dlc_data` for `serialization_version` for when we
break binary level serialization in the DLC protocol spec. This will be done in the 2.0
release of bitcoin-s.

Finally there is a ton of improvements to database handling code.

6cfbf67812 Prevent DB state corruption while accepting the same offer multiple times (#4067)

bce58ba33d Validate announcement signatures on create/accept offer (#4071)

9de4b0272a 2022 01 31 issue 4030 (#4066)

f253b5055e 2022 02 08  dlc accept refactor (#4064)

5aeecdb893 Reworking/refactoring acceptDLCOffer (#4048)

590cd9c72e 2022 02 06 remoteclaimed refactor (#4054)

d213e9935d Add better exception messages (#4053)

bd5bcfef3a 2022 01 18 issue 3969 Add `serialization_version` to `global_dlc_data` (#3992)

8a881b37f4 Add DLC callback for refunded DLC (#3989)

ee0d62c5b8 2022 01 14 `DLCDataManagement` refactor (#3982)

4ca586ca46 2021 12 19 dlc callbacks (#3923)

## Eclair rpc

d71208cf0f Support Eclair v0.6.2 (#3765)

## Esplora

This release adds a new module so you can use esplora on as a chain data source.

03abb7537b Add basic esplora client (#4018)

## fee provider

5f4053b2e4 Create FallbackFeeRateApi (#3974)

bf8b165fe9 Implement recovery for when we cannot receive a fee rate from a FeeRateApi (#3975)

## gui

Small bug fixes. We are moving to having a web interface for bitcoin-s.

This can be found here:

https://github.com/bitcoin-s/bitcoin-s-ts/tree/master/wallet-server-ui

546e030dde [GUI] Fix 'First and last points must be endpoints' exception (#4029)

2aa6f168eb Implement rpc password in GUI (#3986)

7f9fb87a55 Call trim on peer address in Accept dialog (#3942)

856e455be3 Remove 'not set' rate of return % sign on wallet (#3828)

## keymanager

## Lnd rpc

Upgrades lnd to 0.14.2, implements more lnd rpc functionality.

44373f8449 Add functions for LND subscription (#4062)

afc6a32c62 Update lnd to v0.14.2 (#4040)

3ba8fb6dd4 Allow creating invoices with description hash (#3966)

f54ed98c74 Add subscribe invoices function to lnd (#3860)

09f0ac4657 Update lnd to v0.14.1 (#3848)

155301fc1d Allow lnd remote to work with certificate string (#3840)

079fa62073 Lnd v0.14.0 (#3835)

## node

Fixes various bugs around broadcasting witness transactions. Also fixes bugs to make sure
we fetch the entire witness transaction (witnesses included). Previously we wouldn't get the witness
which leads to problems when calculating DLC outcome state.

This release also adds the ability to store peers we see on the p2p network in teh database. Eventually
we will be able to connect to random peers we see on the p2p network rather than just connecting
to the node configured in `bitcoin-s.node.peers`

98c5d816ac 2022 01 25 issue 4014 (#4015)

dbfd58da86 Add log txids inside of inventories in big endian rather than little endian (#4013)

a58ef1cd02 Storing peers in database (#3773)

90e01d7fc6 Fix broadcasting witness vs legacy txs (#3841)

9701769125 Handle TypeIdentifier.MsgWitnessTx messages (#3836)

fc09f41db2 Request witness versions of transactions from nodes (#3829)

## Oracle explorer client

## Oracle server

## secp256k1jni
Updates to the underlying libsecp256k1 to give better performance.

9c9a0a618f Update secp256k1-zkp (#3856)

## wallet

Improves database handling code in the wallet so we use database transactions more often.

This release included various bug fixes and performance.

dc47c070a2 Fix SpendingInfoDAO.findOutputsReceived() bug (#4090)

eeabe5cd77 2022 02 11 clearutxoandaddresses for account transactional (#4077)

883b01006d Fetch blockHeight earlier in TransactionProcessing.processBlock() (#4025)

8d04ca1cd3 2022 01 30 optimize processblock (#4024)

cf16d93648 Fix bug where we didn't set spendingTxId when transitioning from `Reserved` -> `PendingConfirmationsSpent` (#3909)

42d6955f79 Filter dust from coin selection (#3866)

21c97bba12 Filter transactions for onTransactionProcessed (#3990)

41b96c4c7e Add `rescan` field to `walletinfo` response (#3933)

665f931721 Work around for WalletAppConfig.scheduler being blocked by AddressQueueRunnable (#3917)

2d9d12816b Add WalletCallbacks.onBlockProcessed() (#3912)

1969056372 Add better logs for fundTransactionInternal (#3911)

8307fdc0d1 2021 12 12 wallet doublespend tests (#3898)

174c511cd3 2021 12 12 wallet optimization (#3897)

d060c1c5fa Use bigendian txids in wallet log messages (#3893)

174c511cd3 2021 12 12 wallet optimization (#3897)

d060c1c5fa Use bigendian txids in wallet log messages (#3893)

ba88fb1a03 2021 11 12 fix multiple tag issue (#3822)

b3d61bc793 Add deleting address tags to to clearAllUtxosAndAddresses() (#3817)

31e8324522 Make sure exception is caught by Future inside of UtxoHandling.unmarkUTXOsAsReserved() (#3816)

## testkit

## tor

Upgrades tor to 0.4.6.9

1708add6ec Make Tor connect error message more human readable (#4078)

2dcbe73504 Update Tor version to 0.4.6.9 (#3993)

169222a306 Add default proxy params (#3863)

e02c9bba12 Overridable Tor config (#3780)

## Website

95d9bf44a5 Add 1.9.0 of the website (#4096)

e8ee5d7339 Bump website dependencies (#4095)

3931897e7f 2022 02 13 issue 3405 (#4089)

07fcfd0568 2022 02 12 update mdoc fix downloadlink (#4086)

e5707d5002 Fix broken website examples (#4085)

526ed37a28 Cleanup docs that use $ (#4083)

4ea87c741f 2022 02 12 cleanup example config (#4082)

35dccd88d6 Add new section to README for running bitcoin-s, add link to the web frontend (#4069)

99418c798a 2022 02 07 release note draft (#4060)

## Dependencies

6001da3d59 Add documetnation for setting and getting an oracle name (#3782)

c5c76ea46b Add missing markdown files (#3779)

6f696cab78 Update website dependencies (#3771)

c0f2aa73db Fix 1.8.0 of website (#3770)

e61ff11619 Update junit-interface to 0.13.3 (#3951)

13f5fb8dcb Update javafx-base, javafx-controls, ... to 18-ea+10 (#3998)

011fdc2cdb Update akka-grpc-runtime_2.12, ... to 2.1.3 (#3997)

635bc453e4 Update javafx-base, javafx-controls, ... to 18-ea+9 (#3960)

8bc419ff88 Bump shelljs from 0.8.4 to 0.8.5 in /website (#3985)

e520b492bb Update slf4j-api to 1.7.33 (#3981)

44a66e72e5 Upgrade to Scala 2.13.8 (#3789)

2e49bedf7d upgrade to scala-js 1.8.0 (#3979)

e095a1b225 Update sbt-bloop to 1.4.12 (#3977)

af250fcdd3 Update sbt to 1.6.1 (#3949)

bfee5f7e8d Update sbt to 1.6.0 (#3945)

62852a3d19 Update sbt-scalafmt to 2.4.6 (#3940)

1ec444da00 Update scalamock to 5.2.0 (#3943)

0c134c7e9b Update logback-classic to 1.2.10 (#3931)

0c625346e7 Update sbt to 1.5.8 (#3929)

f6608f49b2 Update akka-actor, akka-discovery, ... to 2.6.18 (#3925)

4015909633 Update metrics-core to 4.2.7 (#3922)

b200b1cc16 Update akka-grpc-runtime_2.12, ... to 2.1.2 (#3918)

fa5588d111 Update logback-classic to 1.2.9 (#3914)

2dbc6aa59e Update metrics-core to 4.2.6 (#3908)

0bd59ec8e1 Update sbt to 1.5.7 (#3907)

d1f191bfc6 Update logback-classic to 1.2.8 (#3902)

52ca1d8644 Update sbt to 1.5.6 (#3892)

38a1551bf8 Update metrics-core to 4.2.5 (#3889)

b2bf190dce Update bcprov-jdk15on to 1.70 (#3861)

d0db9ba633 Update scodec-bits to 1.1.30 (#3855)

ea6ac56fd5 Update sbt-native-packager to 1.9.7 (#3833)

65ab6206b3 Update logback-classic to 1.2.7 (#3815)

31ce7cbd77 Update akka-grpc-runtime_2.12, ... to 2.1.1 (#3809)

86bd52d7f2 Update scala-collection-compat to 2.6.0 (#3807)

5b8b296059 Update sbt-scoverage to 1.9.2 (#3791)

8c64c2d094 Update sbt-bloop to 1.4.11 (#3792)

4f081b9d22 Update akka-http, akka-http-testkit, ... to 10.2.7 (#3790)

3c2e07d24e Update junit-interface to 0.13.2 (#3781)

ee348c8a57 Update postgresql to 42.3.1 (#3784)

e5c6c4463c Update sbt-unidoc to 0.5.0 (#3777)

21be9241b8 Update sbt-mdoc to 2.2.24 (#3772)

1310177de4 Update sbt-bloop to 1.4.10 (#3776)

9fe9daaab9 Update postgresql to 42.3.0 (#3766)