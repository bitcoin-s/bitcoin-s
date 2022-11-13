# 1.9.6

This release is backwards compatible with the 1.9.x series of bitcoin-s

See the individual module sections for more information on lower level updates to the codebase.

Want to get started quickly? See our `docker-compose.yml` file. [See instructions here](https://github.com/bitcoin-s/bitcoin-s/#docker)

If you are a typescript developer, [you can access the backend via our typescript library](https://github.com/bitcoin-s/bitcoin-s-ts)

# Executive Summary

This release adds network notifications via the websocket when various tor interactions fail when negotiating a DLC.

This release delete all DLCs that are settled using the `Alpha` version of the DLC protocol.
This makes upgrading to the new v0 format of the dlc protocol easier for an implementation point of view.
This will not delete alpha DLCs that are still in progress, rather throw an exception on wallet startup.

This release also fixes a bug in the wallet where utxos would be stuck in an unconfirmed state.

## Running bitcoin-s

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-1.9.6.zip` and then run it with `chmod +x ./bin/bitcoin-s-server && ./bin/bitcoin-s-server` to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-1.9.6.zip` folder and start using the `bitcoin-s-cli` like this:

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

# Modules

## app commons

d0dac09ce2 Add AddressType json reader (#4851)

## App server

Fixes a bug where we did not categorize `Alpha` version DLCs correctly.
The intent with the 1.9.6 release was to delete these DLCs for ease of upgrading
to the new DLC protocol. This release fixes this categorization bug, and then deletes the DLCs
that were not previously flagged as `Alpha`.

0035c6437b Fix alpha dlc categorization, delete dlcs that are alpha again (#4879)
24e7bcfd97 Fix race condition in websocket tests for dlc node updates (#4866)

## bitcoind rpc

Remove support for `v17` and `v18` bitcoind-rpc clients. 

fff7b19e4a Handle failed getting version of bitcoind (#4869)
746635a551 Remove v18 from `bitcoind-rpc` (#4845)
1d1af1d52e Add generate helper function for bitcoind (#4852)
2482eb939a remove `v17` bitcoind rpc (#4822)
9f53b4e572 Add address type to `createmultisig` bitcoind rpc (#4828)

## bundle

Removes the bundled app with the javafx GUI. Use the electron app now.

7d809bf5df Remove legacy GUI, this fixes CI merges to master as we no longer use the deprecated notary tool from apple (#4870)

## Build

## chain

## clightning rpc

6683259652 Try upgrading clightning tests to newest bitcoind (#4829)

## Core

Add data structures to make sure `Vector[TLVPoint]` and `Vector[DLCPayoutCurvePiece]`
follow an ordering. 

cc8d327ea5 Support bip32 paths with 'h' instead of an apostrophe (#4881)
4fa31b1bc2 Add OrderedDLCPayoutCurvePieces (#4875)
92613709aa Add OrderedTLVPoints as this is an invariant in the codebase in DLCPayoutCurve (#4874)
5111bccc5e Add test for another tx that broke lnd/btcd (#4868)
8431b697a8 Add test for parsing super large transaction (#4849)
994ee25733 Fix empty witness from companion object to case class (#4823)

## Crypto

## db commons

## DLC node

## DLC Oracle

## DLC wallet

## gui

## fee rate

## keymanager

## Lnd rpc

Bump lnd to 0.15.4 to fix transaction parsing bugs.

7a64accd69 Bump lnd to v0.15.4 (#4882)
2a30232181 Update lnd to v0.15.3-beta (#4850)
a4e38c30b4 Add helper functions for lnd chain notifications (#4846)

## Lnurl

## node

Now discover peers on the p2p network by default.
The hope is to move our applications away from connecting to
suredbits peers by default in future releases. This release
will help seed nodes with a list of peers. 

ec596ec51b Enable peer discovery by default (#4862)
f483e356cb Adjust period of time we query for peers on the p2p network from 12 seconds -> 1 hour (#4847)

## Oracle Explorer Client

## wallet

## testkit-core

## testkit

Refactors to remove `v17` and `v18` bitcoind-rpc. 

1a220a3937 2022 10 14 v19 testkit refactor (#4843)
2a617558dd Fix type signature (#4830)

## tor

Update the tor binary to `0.4.7.10` and improve logging. 

4c0e6d5201 Drop support for Tor v2 (#4864)
63b05e398a Rework tor exceptions to be more useful (#4854)
89a4c9e13e Improve tor logging (#4853)
f4fff050f2 Tor 0 4 7 10 (#4848)

## Website

47d2d5a711 Bump README versions to 1.9.6 (#4863)

## Dependencies

4da3e295c4 Update sqlite-jdbc to 3.39.4.0 (#4880)
f381941b1d Update junixsocket-core to 2.6.1 (#4876)
322979c3af Update scalapb-runtime to 0.11.12 (#4877)
17fc49c772 Remove javafx deps (#4873)
07533adaa3 Upgrade scalajs deps now that we have scalajs 1.11.0 (#4871)
e9b4c779fa Upgrade scalajs to 1.10.1 (#4464)
f8ccead8b4 Upgrade sbt to 1.7.3 (#4867)
786f77bfb9 Upgrade sbt ci release to 1.5.11 (#4865)
0c3513b741 Update gson to 2.10 (#4860)
1bd9246097 Update junixsocket-core to 2.6.0 (#4861)
49a7db3803 Update junixsocket-core to 2.5.2 (#4833)
9c05779273 Update bcprov-jdk18on to 1.72 (#4835)
cc79f9bf86 Update sbt-assembly to 2.0.0 (#4855)
deaa3c0e9e Update client3:core to 3.8.3 (#4857)
db74f69df9 Update sbt-bloop to 1.5.4 (#4831)
963646c17e Update slf4j-api to 2.0.3 (#4842)
a8d81a761a Update logback-classic to 1.4.4 (#4832)
fc348da85d Update scalatest to 3.2.14 (#4840)
a2a3eb187f Update sbt-mdoc to 2.3.6 (#4839)