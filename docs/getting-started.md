---
id: getting-started
title: Intro and Getting Started
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

## If you want to setup Bitcoin-S locally

Then go to [this document](getting-setup.md).

## Getting prebuilt artifacts

If you want to add Bitcoin-S to your project, follow the
instructions for your build tool

### Jars

Add this to your `build.sbt`:

```scala
libraryDependencies +="org.bitcoin-s" % "bitcoin-s-secp256k1jni" % "@STABLE_VERSION@"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-core" % "@STABLE_VERSION@"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-chain" % "@STABLE_VERSION@"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-bitcoind-rpc" % "@STABLE_VERSION@"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-eclair-rpc" % "@STABLE_VERSION@"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-key-manager" % "@STABLE_VERSION@"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-node" % "@STABLE_VERSION@"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-wallet" % "@STABLE_VERSION@"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-testkit" % "@STABLE_VERSION@"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-zmq" % "@STABLE_VERSION@"
```

```scala mdoc:passthrough
val isUnstable = "@UNSTABLE_VERSION" != "@STABLE_VERSION@"
if (isUnstable) {
    println(s"""
### Nightly builds

You can also run on the bleeding edge of Bitcoin-S, by
adding a snapshot build to your `build.sbt`. The most
recent snapshot published is `@UNSTABLE_VERSION@`.

""")
}
```

To fetch snapshots, you will need to add the correct
resolver in your `build.sbt`:

```sbt
resolvers += Resolver.sonatypeRepo("snapshots")
```

The official maven repo for releases is

https://repo1.maven.org/maven2/org/bitcoin-s/

The repo for snapshots, which are published after everytime something is merged to master:

https://oss.sonatype.org/content/repositories/snapshots/org/bitcoin-s/

### Docker

We publish docker images to docker hub on every PR merge and tag on github.
You can obtain the images for both the app server and oracle server on these
docker hub repos

[bitcoin-s docker hub repo](https://hub.docker.com/r/bitcoinscala/bitcoin-s-server/tags?page=1&ordering=last_updated)

[oracle-server docker hub repo](https://hub.docker.com/r/bitcoinscala/bitcoin-s-oracle-server/tags?page=1&ordering=last_updated)

## Building JARs yourself

If you want to build Bitcoin-S JARs yourself, you need to use the
[sbt](https://www.scala-sbt.org/) build tool. Once you have sbt
installed, run `sbt publishLocal`. This places the required JAR
files in your `.ivy2/local` folder. On Linux, this is located at
`$HOME/.ivy2/local/` by default.
