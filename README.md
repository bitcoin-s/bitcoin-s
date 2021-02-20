![Bitcoin-S logo](website/static/img/bitcoin-s-dark-logo.png)
[![Build Status](https://github.com/bitcoin-s/bitcoin-s/workflows/Release/badge.svg)](https://github.com/bitcoin-s/bitcoin-s/actions) [![Coverage Status](https://coveralls.io/repos/github/bitcoin-s/bitcoin-s/badge.svg?branch=master)](https://coveralls.io/github/bitcoin-s/bitcoin-s?branch=master) [![Maven Central](https://img.shields.io/badge/Maven%20Central-0.5.0-brightgreen.svg)](https://mvnrepository.com/artifact/org.bitcoin-s) [![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/bitcoin-s-core)

Feature-rich toolkit for making Bitcoin and Lightning applications
on the JVM.

For a complete guide on how to get started with Bitcoin-S, see our website at
[Bitcoin-S.org](https://bitcoin-s.org)

### Getting started

https://bitcoin-s.org/docs/getting-started

### Adding bitcoin-s to your library

The latest release of bitcoin-s is `v0.5.0`, here is how you can use the dependencies in your projects:

```
libraryDependencies += "org.bitcoin-s" % "bitcoin-s-secp256k1jni" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-core" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-crypto" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-chain" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-dlc-oracle" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-db-commons" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-fee-provider" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-bitcoind-rpc" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-eclair-rpc" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-key-manager" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-node" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-wallet" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-testkit" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-zmq" % "0.5.0"

```

#### Docker images

We publish docker images to docker hub on every PR merge and tag on github.
You can obtain the images for both the app server and oracle server on these
docker hub repos

[bitcoin-s docker hub repo](https://hub.docker.com/r/bitcoinscala/bitcoin-s-server/tags?page=1&ordering=last_updated)

[oracle-server docker hub repo](https://hub.docker.com/r/bitcoinscala/bitcoin-s-oracle-server/tags?page=1&ordering=last_updated)
