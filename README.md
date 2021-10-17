![Bitcoin-S logo](website/static/img/bitcoin-s-dark-logo.png)
[![Build Status](https://github.com/bitcoin-s/bitcoin-s/workflows/Release/badge.svg)](https://github.com/bitcoin-s/bitcoin-s/actions) [![Coverage Status](https://coveralls.io/repos/github/bitcoin-s/bitcoin-s/badge.svg?branch=master)](https://coveralls.io/github/bitcoin-s/bitcoin-s?branch=master) [![Maven Central](https://img.shields.io/badge/Maven%20Central-1.8.0-brightgreen.svg)](https://mvnrepository.com/artifact/org.bitcoin-s) [![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/bitcoin-s-core)

Feature-rich toolkit for making Bitcoin and Lightning applications on the JVM.

For a complete guide on how to get started with Bitcoin-S, see our website at [Bitcoin-S.org](https://bitcoin-s.org).

### Contents

- [What is bitcoin-s?](#what-is-bitcoin-s-)
- [Is bitcoin-s production ready?](#is-bitcoin-s-production-ready-)
- [Can I trust the code in bitcoin-s?](#can-i-trust-the-code-in-bitcoin-s-)
- [Getting started (non-developers)](#getting-started--non-developers-)
- [Getting setup (developers)](#getting-setup--developers-)
- [Adding bitcoin-s to your library](#adding-bitcoin-s-to-your-library)
- [Docker images](#docker-images)
- [Contributing](#contributing)
- [Good first issues](#good-first-issues)
- [License](#license)


### What is bitcoin-s?

Bitcoin-S is a loosely coupled set of cryptocurrency libraries for the JVM. They work well together, but also can be used independently. 

This project's goal is NOT to be a full node implementation, rather a set of scalable cryptocurrency libraries that use industry standard tools (rather than esoteric tech often found in cryptocurrency) where possible to make the lives of professional software engineers, security engineers, devops engineers and accountants easier. 

We are rapidly iterating on development with the goal of getting to a set of stable APIs that only change when the underlying bitcoin protocol changes.

If you are a professional working a cryptocurrency business and have feedback on how to make your lives easier, please reach 
out on [slack](https://suredbits.slack.com/ssb/redirect) or [twitter](https://twitter.com/Chris_Stewart_5/)!


### Is bitcoin-s production ready?

Please see our latest releases [here](https://github.com/bitcoin-s/bitcoin-s/actions/workflows/release.yml).

Warning! While we try out best to test every pull request in *master*, this branch may not be stable! Bad things can happen to your node! Beware! 

### Can I trust the code in bitcoin-s?

Please audit and verify any and all code in this toolkit for its suitability and validity. This includes reviewing any and all dependencies.

### Getting started (non-developers)

For a complete guide on how to get started with bitcoin-s, see our [Getting started](https://bitcoin-s.org/docs/getting-started).

This link is intended for individuals who are just interested in installing bitcoin-s rather than developing. If you are interested in development, see **Getting setup** below.

### Getting setup (developers)

For a complete guide on how to get setup with bitcoin-s, see our [Getting setup](https://bitcoin-s.org/docs/getting-setup).

This link is intended for setting up development of bitcoin-s. If you want to just install bitcoin-s rather than develop, see **Getting started** above.

### Adding bitcoin-s to your library

The latest release of bitcoin-s is `1.8.0`, here is how you can use the dependencies in your projects:

```
libraryDependencies += "org.bitcoin-s" % "bitcoin-s-secp256k1jni" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-core" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-crypto" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-chain" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-dlc-oracle" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-oracle-explorer-client" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-app-commons" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-db-commons" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-fee-provider" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-bitcoind-rpc" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-eclair-rpc" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-lnd-rpc" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-key-manager" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-node" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-dlc-node" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-wallet" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-dlc-wallet" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-testkit-core" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-testkit" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-zmq" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-tor" % "1.8.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-cli" % "1.8.0"

```

#### Docker images

We publish docker images to docker hub on every PR merge and tag on github.
You can obtain the images for both the app server and oracle server on these
docker hub repos

[bitcoin-s docker hub repo](https://hub.docker.com/r/bitcoinscala/bitcoin-s-server/tags?page=1&ordering=last_updated)

[oracle-server docker hub repo](https://hub.docker.com/r/bitcoinscala/bitcoin-s-oracle-server/tags?page=1&ordering=last_updated)

### Contributing

Bitcoin-S is an open source project where anyone is welcome to contribute. All contributions are encouraged and appreciated, whether that is code, testing, documentation or something else entirely.

See [here](https://bitcoin-s.org/docs/contributing) for more information.

### Good first issues

[Here](https://github.com/bitcoin-s/bitcoin-s/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22) is a list of good first issues that contain bugs which have a relatively limited scope. This is a great place to get started, gain experience, and get familiar with the bitcoin-s contribution process.

### License

Bitcoin-s is MIT licensed, as found in the [LICENSE](LICENSE) file.