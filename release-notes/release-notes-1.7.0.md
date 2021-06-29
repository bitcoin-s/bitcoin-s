## 1.7.0 Installers!

Last commit this was synced through was  0d2bc7a9270b93b32471fe014cd837693fced967


## App commons

c72c5f84e3d Add extranious json readers, writers, and column mappers (#3325)

5a79acb59ce Fix DLCStatusPickler (#3190)

## App server

af9bf210589 Improve logs in BitcoindRpcBackendUtil (#3339)

d6878b02269 Use same network as bitcoind backend (#3285)

7ecf3edbd60 Update utxo states correctly for bitcoind backend (#3276)

5036c419be7 Add from file ability for AddDLCSigsAndBroadcast DLCs (#3219)

46b33ec0b80 Make add sigs broadcast the funding transaction (#3179)

66e160a918a Send txId instead of full serialized transaction (#3175)

55db4fbd6b1 Remove logging requests and responses for directives unless logging is DEBUG level (#3141)

1e87cb0fdec Remove need for bitcoind install with remote (#3114)

11d58813c1b Remove zmq configurations for bitcoind backend setup as they aren't needed (#3080)

8e29b5c6f6a Change outpoint output format for cli commands (#3048)

1cda5cbf1ec Allow remote bitcoind backend (#3034)

507f5c772e7 Remove request rejection duplication in ServerRoute (#3010)

## Build

d669bd58aec Resolve bundle config and read/write in tmp file (#3327)

cb5ec20eacf Silence all scaladoc warnings (#3336)

94081502c83 Remove previous stable version usage for windows builds as for some reason it doesn't work when setting up dev envs for the first time (#3292)

6cd85765a58 Try downgrading the jdk to 15 for linux release builds (#3287)

898ebed4e95 Windows packaging (#3210)

11cef133e02 2021 06 15 issue 3266 (#3269)

4c9f174a4b9 Remove all flyway plugin sbt config (#3215)

8432712dba3 Fix sbt deprecation warnings (#3163)

fcf55df1650 2021 05 19 jpackage bundle release (#3108)

aff2374f455 Update README.md (#3071)

9b06cdd832a 2021 05 07 cleanup build (#3055)

## Bundle

bd877c80a9f Get logging working in bundle  (#3200)

96fc5445727 Fix logging in bundle (#3167)

6c9d82166ee Fix main class in bundle (#3161)

379ffebd9cf Add startup config flow for bundle (#3142)

caf6c2e7243 Enable java app packaging on bundle project (#3144)

80e6a910567 Add application.conf to bundle (#3118)

ba91ba5596d Add assembly instructions for bundle project (#3104)

## Crypto

745e4c89fa2 Removed point multiplication from ECPrivateKey.freshPrivateKey (#3116)

6bc0943a62f Call decompression on public keys less (#2988)

78f4dfb8c66 Pubkey Refactor (#2936)

63a6f9309dc Introduced AsyncAdaptorSign and AdaptorSign traits (#3037)

## Cli

42966b3cbe6 Remove logback from the cli module (#3117)

## Core

a9292fcad80 Add FutureUtil tests (#3126)

d7b753a869d Add invariant and better error message for invalid numeric contract descriptor (#3338)

5685371e119 Sorted and Ordered Vector Type Support (#3310)

9234cf3ca23 Add new BIP 32 leading zero test vectors (#3313)

fc5bb956dcf Try making the dlc fee test suite async (#3227)

bf02e89faa1 2021 05 29 dlc pnl (#3198)

4b42b90784b Multi-threaded DLC CET signature verification (#3176)

eafaff9ee84 Made DLCTxSigner's async signing methods actually multi-threaded (#3122)

87f353b08f9 DLC Adaptor Point Computation Memoization (#3110)

ac3bae403bb Pulled down all remaining non-wallet non-gui code on adaptor-dlc (#3101)

3205e4e2759 Implement createCETsAndCETSigsAsync() to fix performance issue in test (#3089)

58070f41209 Pulled down dlc and dlcTest projects into core and dlcTest (#3068)

aacba1c077a Pulled down core diff from adaptor-dlc (#3038)

a55a97ba6ff Optimize shift operations in Number (#3025)

## Chain

## db commons

88187abf1a9 Add LnInvoice db mapper (#3286)

dee044eb4e2 2021 05 26 uint64 mapper (#3155)

17d11455049 Removed extraneous findAll call from CRUD.updateAll (#3154)

## DLC Wallet

0d2bc7a9270 Remove unneeded asInstanceOf calls in DLCWallet (#3345)

cfe0c2f0d86 Multi wallet support for DLC database (#3289)

ca2ddbb7ec1 Preserve noncedb outcomes when adding oracle (#3273)

e63a12e47f6 Better error message for invalid DLC refunds (#3209)

a526ad14ee1 Fix DLC Accounting errors (#3249)

c2237ab6fec Only unreserve our utxos when canceling a DLC (#3250)

95f6c0d790f Skip adding DLC sigs if we already have them (#3236)

9431be2f25f 2021 06 07 dlc wallet pnl (#3229)

a56086b751a Use nonceDb's outcome to calc oracle outcome (#3217)

2269a052b15 Rework findDLC() (#3214)

43a5c5fc49a Add dlc wallet test to postgres matrix (#3199)

0e701bc9d06 Fix verify funding sigs (#3194)

45d24facee7 Add announcement data tests (#3173)

dd865c73296 Fix serialId columns to use a String instead of Integer (#3170)

fb81552f6d0 Don't fetch all DLC data when canceling DLC (#3159)

80ace5a9119 Use groupByExistingAnnouncements when accepting (#3157)

880c8898dad Upsert remote txs in DLC Wallet (#3150)

be8e965367d DLC Wallet pulldown (#3138)

## Fee rate provider
a98a26c9296 Make mempool.space fee provider network specific (#3316)

41f3cb4dbfc Fix CachedHttpFeeRateProvider (#3069)

## GUI

88b99b03b5a Add view on oracle explorer button (#3328)

05204e6235f Make it so table doesn't reorder on single update (#3321)

b8538f0300c Fix DLC Table View not being updated (#3315)

00e11b49307 Fix rows in ViewDLCDialog (#3275)

63083bf6425 Make getDLCs async to improve performance (#3222)

935354993ba Populate Broadcast DLC GUI (#3260)

65f096f65a6 Populate Sign GUI when choosing a file (#3243)

cd59aff0637 Fix for showing txid on send (#3237)

a61c11acfc8 Implement sharing of actor systems across the GUI and backend app server (#3220)

1af94658815 Update balance on cancel DLC (#3205)

549d840d02f Only update DLC we are viewing (#3211)

deb0862e07e Fix showing reserved balance instead of unconfirmed in GUI (#3207)

d3f827a127f Update DLC Table with scheduler (#3204)

acc3d228d15 Better messaging while syncing headers (#3202)

4f4c342f80b Sort points in numeric gui (#3203)

ccc40350560 Sync Height on screen (#3196)

5580771f03b Add export and copy result buttons (#3193)

7a5e108ff2b Add button to zip datadir in GUI (#3183)

bcda2467efa Add error messages for when DLC GUI functions timeout (#3184)

03a0ca5ee91 Add QR Code to get adress dialog (#3186)

604194293c8 Replace GUI balance thread with a akka scheduler (#3174)

f8d5202974e Add contract info to ViewDLC (#3177)

957c5c3a8a1 Open bundle GUI at last used tab (#3164)

521a1e25556 Remove unused AcceptDLCDialog file (#3158)

180d7dfcd3c Make loading icon appear until server is full started (#3168)

3d728837ee7 Add ability to enter contract info in offer dialogue (#3160)

c7bb783b1a3 DLC GUI pulldown (#3148)

## Lnd rpc
639adf1181d Use PaymentHashTag type in LndRpcClient (#3333)

fafc564da81 Update LND to v0.13.0-beta (#3290)

db486163f97 Remove caveat for supressing 2.12.x warnings on lnd rpc (#3057)

## Node 

7ba7f8b9ba5 Try to add block generate to address in fixture setup to get around compact filter sync edge case (#3231)

41e22b3cbcd 2021 05 23 Sync race condition (#3129)

a104787985c Fix Node.sync() bug that was caused by not starting filter header sync from the current best filter headers block hash (#3092)

## Oracle

2b8ac08cdcb Give oracle ability to sign messages with private key (#3070)

## Oracle explorer Client 

1f43a1910fd Add ExplorerEnv.fromBitcoinNetwork() and use for View on Oracle Explorer siteUrl (#3332)

c3b982726e2 Fix error messages in SbExplorerClient (#3323)

## Secp256k1jni

8374ddf601c Removes dead symlinks for secp256k1 on osx_arm64 (#3279)

b23b5ad55f9 Make sure secp256k1 is published for java8, not the class version of the jdk it was built on (#3145)

8b8066d1f38 Fix windows secp bindings (#3075)

## Server routes

41468763695 Add DatadirUtil to centralize logic for finding our actual datadir (#3171)

## Wallet

fdba5ad6bef Silence warning log if no error (#3314)

bd11c844621 Add ability to sweep wallet (#3274)

aaa7b42ae7c Add unit test we can handle spending funds and receiving funds in same tx (#3185)

5caf7ee38b8 2021 06 10 cache spendinginfodbs for block (#3245)

af8aaa7bad7 2021 06 07 markasspent optimization (#3244)

8574edede71 Move addressDb database read out of inner loop (#3239)

c9798d6842e Test for processing a block we receive and send in (#3189)

701418f89f3 Fix issue 3102 to allow a user to create an offer with an announcement embedded inside a contract info that the user's wallet has seen before (#3140)

cea8802c052 parallelize matching compact filters as this is a bottleneck during IBD (#3137)

68c7bc1040f 2021 05 21 fetch height parallel (#3124)

9c9e27a8f5b Add optimzations for IBD when the wallet is empty, basically skip all logic for matching filters since there is nothing to match against (#3121)

7468cbec238 Optimize updateUtxoConfirmedStates() to fetch confirmations for blocks in parallel (#3094)

72636b7180a 2021 05 09 received utxos (#3063)

f86f90dc32a Add getbalances cli command (#3022)

## Tor 

02c4505948a Initial Tor support (#3043)

## Website
3ef842eca51 Adjust --depth doc from 100 -> 500 (#3300)

97785561bad Add link to installers, add docs for requirement of java9+ for development environments (#3299)

cdee40b379d Fixed bitcoin-s-cli dir location under bin (#3293)

410ea152240 update website, remove references to a DLC specific branch (#3280)

13409d29c3b Fix header on configuration.md (#3153)

73668bb66c3 Remove old ZMQ config from documentation (#3090)

97a854c5bb8 2021 05 07 fix getting setup (#3053)

4381b93afbd 2021 05 03 improve release notes (#3019)

d25dff14b46 Add 0.6.0 website (#3020)


## Dependencies

ea26c8b3a31 2021 06 23 scalafx dep (#3324)

04d937d4f4d Update sqlite-jdbc to 3.36.0 (#3334)

027b72b7bab Bump set-getter from 0.1.0 to 0.1.1 in /website (#3308)

5f5f1b2b1a7 Update metrics-core to 4.2.2 (#3317)

a3e9275303b Update sbt-coveralls to 1.3.1 (#3297)

6415b9c10a5 Update slf4j-api to 1.7.31 (#3294)

5fbdf8297fe Bump postcss from 7.0.35 to 7.0.36 in /website (#3272)

9be6cb11d54 Update postgresql to 42.2.22 (#3282)

e9de8f30a8a Update metrics-core to 4.2.1 (#3284)

afbc22af1cf Update sbt-native-image to 0.3.1 (#3270)

3c8be6e8d90 Update sbt-coveralls to 1.3.0 (#3262)

4f936b2cbf8 Update sbt to 1.5.4 (#3264)

eeb4741d605 Update sbt-scalajs, scalajs-library, ... to 1.6.0 (#3233)

4f4fd111e7b Update akka-actor, akka-discovery, ... to 2.6.15 (#3246)

1215a228572 Update postgresql to 42.2.21 (#3248)

4b2bc379e35 Update bcprov-jdk15on to 1.69 (#3238)

4efe88fcbaa UPdate sbt and scoverage in conjunction (#3216)

0f9024b7ae7 Update akka-grpc-runtime_2.12, ... to 2.0.0 (#3143)

f958b4036c1 Bump otj-pg-embedded to 0.13.14 (#3228)

a0ce337b22b Upgrade scala versions on ci to 2.12.14 and 2.13.6 (#3226)

1402d1a381a Update scala-library to 2.12.14 (#3187)

c605a02c28f Update sbt-assembly to 0.15.0 (#3109)

ed7a44a1a54 Update sbt-scoverage to 1.8.1 (#3111)

f460f0e79dc Update sbt-assembly to 0.14.10 (#3106)

25852e48d8f Update scala-java-time to 2.3.0 (#3093)

a0453ad660a Update scala-library to 2.13.6 (#3097)

9dfc99cd403 Update scalatest to 3.2.9 (#3091)

6f0fb0671e9 Update metrics-core to 4.2.0 (#3087)

6f8ecf0654a Update scala-collection-compat to 2.4.4 (#3083)

d9a8a38fafe Update sbt-mdoc to 2.2.21 (#3086)

6fd869bd879 Update sourcecode to 0.2.7 (#3084)

bb1eec39416 Update scodec-bits to 1.1.27 (#3085)

b73607fbbb2 Update sbt-scoverage to 1.8.0 (#3066)

d223829a373 Update sbt to 1.5.2 (#3062)

526d8d345da Update scalacheck to 1.15.4 (#3060)

3a0b1af7595 Update janino to 3.1.4 (#3058)

0668ae541fe Update sbt-scoverage to 1.7.3 (#3061)

b854f7b16a2 Fix javafx on mac osx new m1 arch (#3041)