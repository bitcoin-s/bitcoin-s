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

## REPL

You can try out Bitcoin-S in a REPL in a matter of seconds. Run the provided
["try bitcoin-s"](https://github.com/bitcoin-s/bitcoin-s-core/blob/master/try-bitcoin-s.sh)
script, which has no dependencies other than an installed *Java 8*. The script
downloads and installs [Coursier](https://get-coursier.io/) and uses it to
fetch the [Ammonite](https://ammonite.io) REPL and the latest version of
Bitcoin-S. It then drops you into immediately into a REPL session.

```bash
$ curl -s https://raw.githubusercontent.com/bitcoin-s/bitcoin-s/master/try-bitcoin-s.sh | bash
Loading...
Welcome the Bitcoin-S REPL, powered by Ammonite
Check out our documentation and examples at
https://bitcoin-s.org/docs/getting-started
@ val priv = ECPrivateKey()
@ val pub = priv.publicKey
@ val spk = P2WPKHWitnessSPKV0(pub)
@ val address = Bech32Address(spk, MainNet)
@ address.value # Tada! You've just made a Bech32 address
res4: String = "bc1q7ynsz7tamtnvlmts4snrl7e98jc9d8gqwsjsr5"
```

## Getting prebuilt JARs

If you want to add Bitcoin-S to your project, follow the
instructions for your build tool

### sbt

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

### Mill

TODO

## Building JARs yourself

If you want to build Bitcoin-S JARs yourself, you need to use the
[sbt](https://www.scala-sbt.org/) build tool. Once you have sbt
installed, run `sbt publishLocal`. This places the required JAR
files in your `.ivy2/local` folder. On Linux, this is located at
`$HOME/.ivy2/local/` by default.
