[![Build Status](https://travis-ci.org/bitcoin-s/bitcoin-s-core.svg?branch=master)](https://travis-ci.org/bitcoin-s/bitcoin-s-core) [![Coverage Status](https://coveralls.io/repos/github/bitcoin-s/bitcoin-s-core/badge.svg?branch=master)](https://coveralls.io/github/bitcoin-s/bitcoin-s-core?branch=master) [![IRC Network](https://img.shields.io/badge/irc-%23bitcoin--scala-blue.svg "IRC Freenode")](https://webchat.freenode.net/?channels=bitcoin-scala)[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/bitcoin-s-core)

# Bitcoin-S

## Design Principles

- Immutable data structures everywhere
- [Algebraic Data Types](https://en.wikipedia.org/wiki/Algebraic_data_type) to allow the compiler to check for exhaustiveness on match statements
- Using [property based testing](http://www.scalatest.org/user_guide/property_based_testing) to test robustness of code
- Minimize dependencies to reduce attack surface

## Projects

1. `core` - this is where protocol data structures live, like [Transactions](core/src/main/scala/org/bitcoins/core/protocol/transaction/Transaction.scala), [Blocks](core/src/main/scala/org/bitcoins/core/protocol/blockchain/Block.scala), or [PrivateKeys](core/src/main/scala/org/bitcoins/core/crypto/ECKey.scala). For more info read [`core/README.md`](core/README.md)

2. `core-test` - this is where all test cases for the `core` project live

3. `bitcoind-rpc` - this is a RPC client implementation for `bitcoind`. For more info read [`bitcoind-rpc/README.md`](bitcoind-rpc/README.md)

4. `bitcoind-rpc-test` - this is where all test cases for the `bitcoind-rpc` project live

5. `eclair-rpc` - this is a RPC client implementation for [Eclair](https://en.wikipedia.org/wiki/Algebraic_data_type), which is a Lightning Network implementation. For more information please read [`eclair-rpc/README.md`](eclair-rpc/README.md)

6. `eclair-rpc-test` - this is where all test cases for the `eclair-rpc` project live

7. `bench` - benchmarks for Bitcoin-S. For more information please read [`bench/README.md`](bench/README.md)

8. `testkit` - This is a useful testkit for testing Bitcoin related applications. You can spin up Bitcoin and Lightning nodes arbitrarily and set them in specific states. For more information please read [`testkit/README.md`](testkit/README.md)

9. `zmq` - `bitcoind` has a setting that publishes information about the state of the network over ZMQ. This project implements a subscriber that allows you to read and parse that information. For more information see [`zmq/README.md`](zmq/README.md) as well as the official [Bitcoin Core ZMQ documentation](https://github.com/bitcoin/bitcoin/blob/master/doc/zmq.md)

10. `secp256k1jni` - JNI (Java Native Interface) for [secp256k1](https://github.com/bitcoin-core/secp256k1), a optimized C library for EC operations on curve secp256k1. It has support for a wide range of cryptographic operations used in the Bitcoin protocol. Fore more information please read [`secp256k1jni/README.md`](secp256k1jni/README.md)

## REPL

In any given sub project, it's possible to open a REPL session. This session has both main and test sources from Bitcoin-S available, as well as all dependencies for the given sub project. To do this: 

```scala 
// core project 
$ sbt coreTest/test:run // we do coreTest instead of core to have all test sources available

// this works as well
$ sbt
> project coreTest
> amm
...
Loading...
Compiling (synthetic)/ammonite/predef/interpBridge.sc
Compiling (synthetic)/ammonite/predef/replBridge.sc
Compiling (synthetic)/ammonite/predef/DefaultPredef.sc
Welcome to the Ammonite Repl 1.6.2
(Scala 2.12.7 Java 1.8.0_191)
If you like Ammonite, please support our development at www.patreon.com/lihaoyi
@ import org.bitcoins.core.protocol.ln.currency.MilliSatoshis 
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis

@ MilliSatoshis(100) 
res1: MilliSatoshis = MilliSatoshisImpl(100)


// Bitcoind RPC project
$ sbt bitcoindRpcTest/test:run

// this works as well
$ sbt
> project bitcoindRpcTest
> amm

// Similarly for other projects
```


## Artifacts

You need to add the Bitcoin-S Bintray to your resolvers to be able to access published artifacts.

### sbt

With sbt, this can be done like this:

```scala
resolvers += Resolver.bintrayRepo("bitcoin-s", "bitcoin-s-core"),
```

Now you should be able to add Bitcoin-S artifacts like this:

```scala

"org.bitcoins" % "bitcoin-s-secp256k1jni" % "0.0.4"

"org.bitcoins" %% "bitcoin-s-core" % "0.0.4" withSources() withJavadoc()

"org.bitcoins" %% "bitcoin-s-bitcoind-rpc" % "0.0.4" withSources() withJavadoc()

"org.bitcoins" %% "bitcoin-s-eclair-rpc" % "0.0.4" withSources() withJavadoc()

"org.bitcoins" %% "bitcoin-s-testkit" % "0.0.4" withSources() withJavadoc()

"org.bitcoins" %% "bitcoin-s-zmq" % "0.0.4" withSources() withJavadoc()
```

### Ammonite

> Ammonite is (among other things) a modernized Scala REPL with syntax highlighting, multi-line editing, the ability to load artifacts directly in the REPL, and many other quality-of-life improvements missing in the default Scala REPL.
>
> Ammonite is a project by Li Haoyi, and you can get it at [ammonite.io](https://ammonite.io)

With Ammonite, this can be done like this:

```scala
@ import coursier.MavenRepository
import coursier.MavenRepository

@ interp.repositories() ++= Seq(MavenRepository("https://dl.bintray.com/bitcoin-s/bitcoin-s-core"))

@ import $ivy.`org.bitcoins::bitcoin-s-core:0.0.4`
import $ivy.$

@ import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.currency.Bitcoins

@ Bitcoins(10)
res0: Bitcoins = BitcoinsImpl(10) // 🎉
```

This is only necessary one time, Ammonite remembers your resolvers across sessions.

### Published artifacts

Versioned artifacts are available online.

Snapshots (not necessarily stable, use for local development) are available at
[JFrog](https://oss.jfrog.org/webapp/#/artifacts/browse/tree/General/oss-snapshot-local/org/bitcoins).

Tagged versions are available at [Bintray](https://bintray.com/beta/#/bitcoin-s/bitcoin-s-core?tab=packages).
