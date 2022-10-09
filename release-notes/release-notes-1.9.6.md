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

## App server

## bitcoind rpc

## bundle

## Build

## chain

## Core

## Crypto

## db commons

## DLC node

de43dadf52b Network notifications (#4774)

## DLC Oracle

## DLC wallet

62081a43ecd 2022 10 05 Delete legacy `DLCSerializationVersion.Alpha` DLCs for a cleaner upgrade to v0 spec (#4817)

## gui

## fee rate

## keymanager

## Lnd rpc

## Lnurl

## node

718053668d8 2022 10 07 node test fixes (#4819)

## Oracle Explorer Client

## wallet

Fix bug where transactions would be stuck in a unconfirmed state

ddc672cc466 Fix unconfirmed -> confirmed state change (#4816)

## testkit-core

## testkit

## tor

## Website

## Dependencies

c075112db5b Upgrade sbt to 1.7.2 (#4818)