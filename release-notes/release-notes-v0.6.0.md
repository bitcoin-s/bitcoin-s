## Docker support

d315b5028b4 update Base docker image to a ubuntu buster (#2799)

145682d2e8d Update docs to use the latest docker image names (#2758)

4dd356fe190 Enable 'dockerUpdateLatest' option to give us the latest tag on publishing artifacts (#2752)

f7d14f35e48 Rework docker configuration to pass in a custom configuration file (#2718)

2532de09193 2021 02 19 dockerhub docs (#2693)

eb005b71812 Add github workflow steps to publish to dockerhub (#2684)

a20b87788ba 2021 02 15 appserver docker (#2673)

5de4537f411 Get basic docker image working with oracle server (#2668)



## New modules

### CoreJS & CryptoJS

4ed873194cb Enable scalajsbundler plugin on coreJS (#2853)

abcf78cf6e4 Enable publishing of scalajs artifacts (#2849)

478ab16e28c Get all of Core working with JS (#2826)

441035eeb1e Silence scalajs warnings for org.bitcoins.crypto package (#2822)

35d2670c3f2 Move hard coded test vectors from resource files into scala files (#2818)

e16dec91c91 Adaptor signatures for Scala.js (#2794)

e82213f9474 Schnorr js (#2805)

f593cf063e5 Remove logging from core (#2810)

1ab58c5977a SipHash for Scala.js (#2797)

342510012a2 WIP: Implement bcrypto facades (#2743)

c43e1db3796 Add new JS projects to list in build.sbt (#2761)

3143478f4b7 Skip publishing of js projects (#2734)

89321570d46 Resturcutre cryptoTest & coreTest to work with scalajs build (#2731)

c8fd4ab5575 2021 02 27 dersignatureutil mv (#2730)

980f5237ff3 Refactor crypto module to be compatible with Scala.js part 1 (#2719)

a1bdbda039a CryptoRuntime abstraction (#2658)


### TestkitCore

f256636565e Remove logging from testkit core (#2813)

0de536de3bd Add testkit-core module (#2726)

### Lnd rpc client

68034600bd2 Fix parsing comments in LndConfig (#2864)

c0e6a7e5a12 Add sendouputs function to lnd rpc (#2858)

656e0928bf1 Inital LND rpc with some tests (#2836)

### AsyncUtil

0832f2d9ba1 Move tests out of bitcoindRpcTest that belong in async-utils (#2796)

fb599e0d121 2021 03 09 async utils tests (#2781)

bb49a1f16d6 2021 02 25 async utils (#2725)

### Suredbits Oracle Explorer Client


26919243487 Rework oracle explorer client to use new api paths (#2866)

07dde5f0a73 Add helper functions for hashing annoucements for SbExplorerClient (#2861)

ad738a5a283 Implement Oracle Explorer Client (#2838)


## Core

32a6db930bd Made ECPrivateKey signing synchronous and got src compiling (#2652)

d8f1b1e03c3 Attempt to find type name when parsing incorrect tlv type (#2820)

147a6537fd8 Implement bech32m (#2572)

fcb5dbeaa9f Add Broadcast TxoState (#2735)

207856b0c42 Completely remove range event descriptors (#2764)

0d7edb7a681 2021 02 21 cheap redeemscript check (#2707)

7d4314e28d7 2021 02 20 number byte representation (#2703)

6132d20e3a0 Decrease false positive rate to avoid spurious CI failures (#2698)

f6b48e1824e Fix normalized string comparison (#2695)

942e31d60e4 Optimized sigPoint computation to use non-custom secp functions (#2665)

8f24377c79d Compute `sigPoint`s eagerly but asynchronously (#2642)

7f5e9b4cc98 Use specific functions for Oracle Signing version (#2659)

95163d51146 Refactor HDCoinType to be ADT (#2657)

9fdf4bca493 Create ScriptFactory.isValidAsm() to standardize how check validity o… (#2629)

58a46fdfdaa Add number cache trait, use it in all number types (u8,u32,etc) and S… (#2627)

de30c61f546 Do cheap checks in predicates first before more expensive ones (#2628)

26e481a6e48 Re-wrote CETCalculator.splitIntoRanges (#2621)

f815eb7f77b Added utilities to created linear approximations of Long => Long functions (#2537)

## Chain

89185338e04 Refactoring `chain` (#2662)

## Wallet

8ce22583a55 Wallet Rebroadcast Transactions thread (#2711)

7a88f585340 Reduce fee rate for spending coinbase utxos (#2815)

b90a8250427 Move blockhash to tx table from spending info table (#2744)

98da2516b22 Begin re-introducing parallelism in the wallet to make everything faster (#2705)

cd1e2e1b5d3 Reduce usage of .findAll() (doesn't scale for large dbs). Now pass in… (#2706)

97f5c1d14f3 Allow implicit execution context to be passed in to RescanHandling.findMatches() & RescanHandling.fetchFiltersInRange() (#2704)

7357da38115 Bump the timeout for address queue exception test to make sure we get correct exception (#2697)


## Node

## Testkit

eaac9c154c2 Implement caching of bitcoind in the walletTest,nodeTest, and partially bitcoindRpcTest project (#2792)

40e1ab0c01e Add guard for the case when listFiles returns null (#2696)

## DLC oracle

2d3b3b64b8a Fix potential unordered nonces in announcement (#2831)

4a7bd73f4cf Fix DLCOracle to be Network Agnostic (#2749)

0bfc034ae13 Make sure DLCOracleAppConfig creates the oracle directory (#2720)

9d210b7e1f4 Change oracle db to have its own directory (#2667)


## Oracle Server

176cbc9e3ee Add ability to delete Oracle signatures (#2851)

989443bb5f9 Add signed outcome to `getevent` rpc, fix other small api bugs (#2757)

99b9c188ad5 Correct log location and logs for oracle server (#2722)

b20776cb241 Simplify oracle server RPC api (#2656)

6280b2a7ffa Fix docs to use correct oracle server port (#2666)

291efe0e4c2 Give oracle server its own port (#2653)


## Bitcoind rpc

02ab4026b8f Have BitcoindV21RpcClientTest wait for indexes to sync (#2855)

96f658317ed Create NativeProcessFactory, extend it in both Client.scala & EclairRpcClient.scala (#2800)

e0421f657b5 Refactor starting second bitcoind in MempoolRpcTest, remove Thread.sleep (#2776)

ab78b6fa3b3 Wrap entire Client.getPayload() into try catch to avoid exceptions leaking (#2767)

4c65782c62c Cache httpClient in bitcoind, rename Test.akkaHttp -> Test.akkaHttpTestkit (#2702)


## Eclair rpc

## Documentation / Website

2745fdf8db9 Rework the website scaladoc aggregation and website (#2846)

e9cac3d8b19 Add docs for getblockheader (#2811)

827553ac9dd Make website publish work with teh latest stable version (#2766)

7bf00a1a200 Make it clear on the getting-setup.md page that this is only for development, you can find binaries in getting-started.md (#2759)

af8343dd567 2021 02 10 Website fixes (#2643)

b079073094c Fix/typos (#2633)

5d1dace60d5 Update README to have correct latest version (#2631)


## Build

7dcef9b5696 Fix build warnings that came with sbt 1.5.0 (#2857)

0437dcb04d4 Update gitignore file with recommendations from unidoc (#2845)

3f938bf8c31 Bump timeout on bind to avoid spurious ci failures hopefully (#2791)

b0f098ffde6 Turn off parallelExecution and remove extra AsyncUtil test class (#2790)

eef52bb512b Update all deps that failed because of bad build (#2774)

b6b234d0a24 Add fetch depth zero to everything to fix bug introduced in #2766 (#2773)

9b252337db0 2021 03 04 fix publish pt2 (#2763)

4a847c03d6d Fix unidoc issue with scala-js modules, this now ignores them from un… (#2742)

c6bf0bb1a31 Workaround for issue 2708 (#2709)

67303b14107 Set fetch-depth to 100 so we don't take forever to clone repo on ci (#2694)

aa632efb652 Make sure dynver versions use '-' instead of '+' (#2681)

581b3e8ac51 Add --depth 100 restriction when cloning bitcoin-s repo to speed up clone time (#2674)


### Other

ffabfbd6ba8 Update scala-java-time to 2.2.1 (#2862)

be396443f79 Update sbt to 1.5.0 (#2854)

c7ddca7ca36 Update javafx-base, javafx-controls, ... to 17-ea+6 (#2852)

43a65ecabb2 Update sourcecode to 0.2.5 (#2848)

32ef1c30d72 Update scalatest to 3.2.7 (#2843)

d07c04f050d Update sbt-scalajs, scalajs-compiler, ... to 1.5.1 (#2837)

beff619d71d Update scala-collection-compat to 2.4.3 (#2834)

cbf60ea7d3b Update sbt-mdoc to 2.2.19 (#2833)

1df87499417 Update scodec-bits to 1.1.25 (#2835)

cec5feb49e8 Update akka to v10.2.4 (#2832)

5ed33fa5155 Update javafx-base, javafx-controls, ... to 17-ea+5 (#2829)

0aa153f38e4 Update sbt-ci-release to 1.5.7 (#2819)

bcba673fd5a Update sbt-native-packager to 1.8.1 (#2798)

b8140053b91 Update javafx-base, javafx-controls, ... to 17-ea+3 (#2804)

4e21c0dafc2 Update sbt-ci-release to 1.5.6 (#2789)

6094848d814 Update javafx-base, javafx-controls, ... to 17-ea+2 (#2728)

4fd8820230a Update akka-http, akka-http-testkit to 10.1.14 (#2723)

1f63d246295 Update metrics-core to 4.1.18 (#2716)

8dd6df78c4b Upgrade scalac to 2.13.5 (#2713)

9092338493c Update akka-actor, akka-slf4j, akka-stream, ... to 2.6.13 (#2714)

74d0a8564ce Update scalatest to 3.2.5 (#2687)

bc1189ac3e7 Update sbt-bloop to 1.4.8 (#2683)

aa8ee9df481 Update postgresql to 42.2.19 (#2686)

94812a761cf Update scalatest to 3.2.4 (#2677)

3ed9014da6b Update sbt-mdoc to 2.2.18 (#2676)

97ad5e255a2 Update scala-collection-compat to 2.4.2 (#2670)

8fd98f885fe Update scodec-bits to 1.1.24 (#2671)

42a72b7e323 Update scalacheck to 1.15.3 (#2669)

56867900565 Update sbt-bloop to 1.4.7 (#2661)

bcade41326c Update javafx-base, javafx-controls, ... to 16-ea+7 (#2654)

cf90ce3184d Update janino to 3.1.3 (#2559)

f0d21ebb815 Update sbt-mdoc to 2.2.17 (#2632)