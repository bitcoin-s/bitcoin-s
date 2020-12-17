# Highlights
This release stabilizes a lot of existing code. A few of the highlights are fixing a bug in how `LnInvoice` is parsed in #337 and fixes the dev flow with `testkit` in #341. Now it is much easier to develop on bitcoin-s and use `testkit` to test bitcoin related applications.

Another improvement in this release is #308, which fixed issues that some version of mac osx users where having with our native loading of libsecp256k1 into bitcoin-s. For more information how we natively load libsecp256k1 with bitcoin-s see the [README.md](https://github.com/bitcoin-s/bitcoin-s-core/tree/master/secp256k1jni#secp256k1jni). We still need to add native windows support for libsecp256k1.

This release also integrates a [website](https://bitcoin-s.github.io/bitcoin-s-core/latest/api/) for our scaladocs.

Another major theme of this release is improvements to our `EclaiRpcClient`. There are security critical improvements in this release and **it is highly recommended you upgrade**.

# Thanks

Thanks to @nkohen and @torkelrogstad for their contributions to this release.

# Changelog

535fcfb6 (HEAD -> 2019-02-19-readme-update, chris/2019-02-19-readme-update) Update readme with new version
74f12039 (origin/master, origin/HEAD, master) Clean up compiler warnings (#349)
3a0c832d Reduce number of requests we send to eclair in testkits, add commandN… (#343)
f97902b4 (2019-02-19-rm-more-await-result) transformRetryToTestFailure in scala 2.11.12 (#346)
56f12fb3 Add more tests to bump test coverage (#345)
9a8310a2 Better test error messages (#336)
463fe7ee (nkohen/master) Put tests for bitcoind-rpc and eclair-rpc into a separate project to … (#341)
44ea4e1f 2019 02 14 fix digital signature bug ln invoice (#337)
49458457 (bump-scalac-2.12.8) Add BasicArithmetic trait (#329)
b6dc57ed Fix bench project configuration (#338)
8391fadc Bump scalac to 2.12.8 (#340)
f85fcf51 (torkel/master) Add bitcoin-s unit test class, all unit tests going forward should extend this (#335)
e0a38d7e Fix initialization problem with LnInvoice and the separator char (#334)
346f33b7 (2019-02-14-publishing-multiple-scalac-versions-rd2) Add explicit test cases for parsing an htlc (#333)
e8f5abfc (2019-02-10-script-parse-bug-fix) Change Factory to an abstract class (#330)
3200e6a1 Make start methods return Future[Unit] (#328)
248c5005 Get Eclair data dir from config in EclairAuthCredentials (#326)
6d873abb remove response trace log (#325)
378c0d2a Default param in Eclair RPC fromDatadir, tweaks ChannelUpdate RPC type (#324)
f2f70171 rm usage of getSimpleName, as it breaks logback logging schemes (#322)
89e14d3c add logs in eclair rpc client for requests and responses (#321)
87d35593 Fixes conversion bug when sending with Eclair RPC  (#318)
0a3bfbf6 Make timestamp a lazy val outside of versioning code (#317)
f144628f Adds audit RPC call + test (#314)
dfdf0e43 Adds feebaseMsat and proportional fees for ChannelResult (#312)
8ad445c1 Changes how Travis fetches most recent snapshot (#315)
37f096ee updates osx binaries (#308)
5d3bf4fe Add sbt-git plugin, reformat snapshot versioning scheme (#309)
6c935f1c Add network call to eclair api (#310)
1bf39a38 findroute RPC call tests/types (#301)
f5ac7970 Adds README notes on how to add Bitcoin-S in Ammonite (#300)
36c01a0c Scaladoc (#296)
5c9747f9 Update rpc README (#306)
2eae4c28 Tunes scoverage (#303)
26949b02 Adds Bitcoin-S static site (#299)
5a195ec6 Improves types for Eclair RPC (#298)
