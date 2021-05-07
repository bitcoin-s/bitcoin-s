---
id: version-0.6.0-getting-started
title: Intro and Getting Started
original_id: getting-started
---

## Philosophy

Bitcoin-S is a loosely coupled set of cryptocurrency libraries for the JVM. They work well together, but also can be used
independently. This project's goal is NOT to be a full node implementation, rather a set of scalable cryptocurrency libraries
that use industry standard tools (rather than esoteric tech often found in cryptocurrency) where possible to make the lives of professional
software engineers, security engineers, devops engineers and accountants easier.
We are rapidly iterating on development with the goal of getting to a set of stable APIs that only change when the underlying bitcoin protocol changes.

If you are a professional working a cryptocurrency business and
have feedback on how to make your lives easier, please reach out on [slack](https://join.slack.com/t/suredbits/shared_invite/zt-eavycu0x-WQL7XOakzQo8tAy7jHHZUw),
[gitter](https://gitter.im/bitcoin-s-core/) or [twitter](https://twitter.com/Chris_Stewart_5/)!

## Getting prebuilt artifacts

### Java binaries

<details>
Please download these from our latest [release on github](https://github.com/bitcoin-s/bitcoin-s/releases/tag/v0.5.0)

</details>

### Docker

<details>
We publish docker images to docker hub on every PR merge and tag on github.
You can obtain the images for both the app server and oracle server on these
docker hub repos

[bitcoin-s-server docker hub repo](https://hub.docker.com/r/bitcoinscala/bitcoin-s-server/tags?page=1&ordering=last_updated)

[bitcoin-s-oracle-server docker hub repo](https://hub.docker.com/r/bitcoinscala/bitcoin-s-oracle-server/tags?page=1&ordering=last_updated)
</details>

### Library jars

<details>
Add this to your `build.sbt`:

```scala


libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-bitcoind-rpc" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-core" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-chain" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-dlc-oracle" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-eclair-rpc" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-fee-provider" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-key-manager" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-lnd-rpc" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-node" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-oracle-explorer-client" % "0.5.0"

libraryDependencies +="org.bitcoin-s" % "bitcoin-s-secp256k1jni" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-testkit-core" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-testkit" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-wallet" % "0.5.0"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-zmq" % "0.5.0"

```


### Nightly builds

You can also run on the bleeding edge of Bitcoin-S, by
adding a snapshot build to your `build.sbt`. The most
recent snapshot published is `0.0.0-2781-02c45059-20210507-1710-SNAPSHOT`.



To fetch snapshots, you will need to add the correct
resolver in your `build.sbt`:

```sbt
resolvers += Resolver.sonatypeRepo("snapshots")
```

The official maven repo for releases is

https://repo1.maven.org/maven2/org/bitcoin-s/

The repo for snapshots, which are published after everytime something is merged to master:

https://oss.sonatype.org/content/repositories/snapshots/org/bitcoin-s/

</details>

## Building JARs yourself

Please see [our setup docs](getting-setup)

## If you want to setup Bitcoin-S locally for development

Please see [our setup docs](getting-setup)


