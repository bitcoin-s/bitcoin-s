# 1.9.5

This release is backwards compatible with the 1.9.x series of bitcoin-s

See the individual module sections for more information on lower level updates to the codebase.

Want to get started quickly? See our `docker-compose.yml` file. [See instructions here](https://github.com/bitcoin-s/bitcoin-s/#docker)

If you are a typescript developer, [you can access the backend via our typescript library](https://github.com/bitcoin-s/bitcoin-s-ts)

# Executive Summary

This release includes a well defined ordering for nonces. This also fixes some bugs in the postgres backend for the dlc wallet.

## Running bitcoin-s

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-1.9.5.zip` and then run it with `chmod +x ./bin/bitcoin-s-server && ./bin/bitcoin-s-server` to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-1.9.5.zip` folder and start using the `bitcoin-s-cli` like this:

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

# Modules

## app commons

## App server

## bitcoind rpc

9179fe1794 Add way for to handle utxo not existing with bitcoind.getTxOut (#4797)
9518826882 Add blockheight to bitcoind's ListTransactionsResult (#4795)

## bundle

## Build

65c7c6102c Bump versions
87664a3824 Error on invalid download of binaries (#4798)

## chain

bcddb015ae 2022 09 12 Filter header and filter websocket events (#4777)

## Core

e8ebcf55cd Fix bug where signatures were out of order from what was posted in the announcement (#4809)
ecf2d2ba2c Fix bug where we were generating too many nonces (#4805)
8d91abd678 Add signature ordering to `ClaimedDLCStatus.oracleSigs` (#4804)
9c506b639f Add OrderedSchnorrSignatures, use it in OracleAttestment (#4803)
4cb47c4ef4 Remove event descriptor parameter from SigningVersion.calcOutcomeHash() (#4796)
75b034d6e0 Add socket address to NodeUri (#4794)
fecb9902ec 2022 09 20 rm dlc test (#4792)

## Crypto

eb5310f312 Use ByteVector.compare rather than rolling our own (#4814)

## db commons

cf9f48c221 Set db auth in slick db config (#4781)

## DLC node

## DLC Oracle

34e023e93f 2022 09 29 handle unordered sigs (#4807)

## DLC wallet

a1fad6bcc4 Fix bug where postgres tests weren't running on `dlcWalletTest/test` (#4801)

## gui

## fee rate

## keymanager

## Lnd rpc

6b479e8765 Add support for mac m1 lnd rpc (#4780)

## Lnurl

## node

## Oracle Explorer Client

## wallet

cad13182df Use postrgres when flagged in TrezorAddressTest (#4768)

## testkit-core

## testkit

2c85f92b18 Fix bug in `DualWalletTestCachedBitcoind` where we didn't pass `pgUrl` (#4806)
c0443a972d Have EmbeddedPg check PG_ENABLED value (#4791)

## tor

## Website

712bb50f88 1.9.5 of the website (#4812)

## Dependencies

2a0cb57cf1 Update logkit to 20020529 (#4786)
193922c9c5 Update avalon-framework to 20020627 (#4731)
b6d9792364 Update scalafx to 18.0.2-R29 (#4749)
b1017f8db5 Update jna to 5.12.1 (#4742)
6f9cf52aaa Update bcprov-jdk15on to 1.71.1 (#4743)
f48daad974 Update javafx-base, javafx-controls, ... to 20-ea+2 (#4745)
419ccbccbc Update scala-collection-compat to 2.8.1 (#4747)
e10ab428e5 Update scalacheck to 1.16.0 (#4748)
4e1c1764c3 Update slick, slick-hikaricp to 3.4.1 (#4783)
469a379655 Update slf4j-api to 2.0.1 (#4784)
188faf1625 Update client3:core to 3.8.0 (#4776)
7d70885581 Update waffle-jna to 3.2.0 (#4735)
48aed46921 Update sbt-scalajs-bundler to 0.21.0 (#4733)
9287f00a84 Update sbt-bloop to 1.5.3 (#4732)
55ad5f030b Update gson to 2.9.1 (#4736)
cf6015f281 Update zxing:core, zxing:javase to 3.5.0 (#4737)
6a74c012bf Update metrics-core, metrics-healthchecks, ... to 4.2.12 (#4738)
398e4db3f6 Update logkit to 1.2.2 (#4741)
abe2c3d9d2 Update logback-classic to 1.4.1 (#4782)
f98401259a Update sbt-mdoc to 2.3.3 (#4750)
