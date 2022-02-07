# 1.8.0 DLC Wallet Tor negotiation

## Running Bitcoin-S

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-1.8.0.zip` and then run it with `./bin/bitcoin-s-server` to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-1.8.0.zip` folder and start using the `bitcoin-s-cli` like this:

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
$ sha256sum bitcoin-s-server-1.8.0.tgz
aa1084edb5fcd3d1dbcafe0d0fba787abf4cd455bbe38809bd9a65a49c0cd0eb bitcoin-s-server-1.8.0.tgz
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


## App server

## bitcoind rpc


## Build 

## Cli

## chain

## Core

## Db commons

## DLC node


## DLC Oracle


## DLC wallet

## fee provider

## gui 


## keymanager

## Lnd rpc

## node

## Oracle explorer client

## Oracle server

## secp256k1jni

## wallet

## testkit

## tor

## Website

## Dependencies