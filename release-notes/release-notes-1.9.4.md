# 1.9.4

This release is backwards compatible with the 1.9.x series of bitcoin-s

See the individual module sections for more information on lower level updates to the codebase.

Want to get started quickly? See our `docker-compose.yml` file. [See instructions here](https://github.com/bitcoin-s/bitcoin-s/#docker)

If you are a typescript developer, [you can access the backend via our typescript library](https://github.com/bitcoin-s/bitcoin-s-ts)

# Executive Summary

This is a bug fix release. In our 1.9.3 release we did not successfully publish all jars for the libraries.
For more information see: https://github.com/bitcoin-s/bitcoin-s/issues/4725

This release drops support for Scala 2.12.x, which fixes this bug. It also updates some dependencies.

## Running bitcoin-s

If you want to run the standalone server binary, after verifying gpg signatures, you
can `unzip bitcoin-s-server-1.9.4.zip` and then run it with `chmod +x ./bin/bitcoin-s-server && ./bin/bitcoin-s-server` to start the node. You will need to
configure the node properly first, you can find example
configurations [here](https://bitcoin-s.org/docs/config/configuration#example-configuration-file).

You can then unzip the `bitcoin-s-cli-1.9.4.zip` folder and start using the `bitcoin-s-cli` like this:

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

2cfd6f3591 Allow for custom config file name (#4709)

## App server

7b7847885e Attempt to reduce redundant work on startup to speed up start up time (#4764)
7460dcd255 Parallelize running of individual migrations (#4761)
6b432ea509 Emit 2000th header received during IBD (#4719)
a36e55c892 Fix patterns I missed when updating logback (#4713)

## bitcoind rpc

0c05edd633 mask bitcoind password (#4763)
61d4882efd Implement retry of fetching bitcoind version when calling `clientF` (#4760)

## bundle

## Build

7d9d0c577f Run scalafmt (#4757)
7fe9bdbe35 re-add Compile / fork in server.sbt so we can run appServer from sbt console (#4730)
06844bcd13 Make docker publish use java 18 to avoid slick 3.4.0 issues (#4718)
d2fb3fc150 Revert "Make electron build use bitcoin-s-ts latest tag (#4701)" (#4711)
38725f0155 Make electron build use bitcoin-s-ts latest tag (#4701)
3ad43a8f86 ignore jakarta dependnecy in logback (#4707)
328e1653a9 Drop support for scalac `2.12.x` (#4704)
669eb03f93 2022 09 02 issue 4699 (#4700)

## chain

## Core

2f18f622ab Give BitcoinNetworks.knownNetworks proper type (#4766)

## Crypto

## db commons

## DLC node

16893f999e DLC connection checks and notifications (#4720)

## DLC wallet

26595ab3ac Fix bug where no exception was thrown if the acceptor did not have enough money (#4728)

## gui

## fee rate

## keymanager

## Lnd rpc

20ed1dcab0 Fix lnd default datadir (#4767)
8bbfbc89d7 Add handling so test lnd clients can be reached by docker (#4729)
1758197d58 Remove bitcoind as a dep for lnd (#4703)

## Lnurl

## node

fae1a53579 Bump `bitcoin-s.node.query-wait-time=120 seconds` (#4759)

## Oracle Explorer Client

## wallet

81cddc12df Remove checkRootAccount/downloadMissingUtxos (#4762)
2bf1c9d1a5 Try to debug what is happening on CI with zmq test (#4708)
2448fe13e8 Revert the unique outpoint index DB migration (#4652)

## testkit-core

## testkit

018a6e58ee Add [regtest] section to fixture config (#4717)

## tor

1571b85819 Bump tor timeout to 120 seconds (#4723)

## Website

## Dependencies

2d83210ba3 Update flyway-core to 9.2.3 (#4744)
12326c7f33 Update sqlite-jdbc to 3.39.3.0 (#4755)
6bfb669343 Update akka deps (#4724)
fd7bef3aa7 Try out slick 3.4.0-RC3 (#4620)
3ee4fe1138 Upgrade dependencies (#4705)