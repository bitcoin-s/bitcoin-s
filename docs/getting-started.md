---
id: getting-started
title: Add Bitcoin-S to your project
---

## REPL

You can try out Bitcoin-S in a REPL in a matter of seconds. Run the provided
["try bitcoin-s"](https://github.com/bitcoin-s/bitcoin-s-core/blob/master/try-bitcoin-s.sh)
script, which has no dependencies other than an installed JDK. The script
downloads and installs [Coursier](https://get-coursier.io/) and uses it to
fetch the [Ammonite](https://ammonite.io) REPL and the latest version of
Bitcoin-S. It then drops you into immediately into a REPL session.

```
% curl -s https://raw.githubusercontent.com/bitcoin-s/bitcoin-s-core/master/try-bitcoin-s.sh | bash
Loading...
Welcome to the Ammonite Repl 1.0.3
(Scala 2.12.4 Java 1.8.0_152)
If you like Ammonite, please support our development at www.patreon.com/lihaoyi
@ 23 :: "foo" :: true :: HNil
res0: Int :: String :: Boolean :: HNil = 23 :: "foo" :: true :: HNil
%
```

## Build tools

If you want to add Bitcoin-S to your project, follow the
instructions for your build tool

### sbt

Add this to your `build.sbt`:

```scala
libraryDependencies +="org.bitcoins" % "bitcoin-s-secp256k1jni" % "@VERSION@"

libraryDependencies += "org.bitcoins" %% "bitcoin-s-core" % "@VERSION@" withSources() withJavadoc()

libraryDependencies += "org.bitcoins" %% "bitcoin-s-bitcoind-rpc" % "@VERSION@" withSources() withJavadoc()

libraryDependencies += "org.bitcoins" %% "bitcoin-s-eclair-rpc" % "@VERSION@" withSources() withJavadoc()

libraryDependencies += "org.bitcoins" %% "bitcoin-s-testkit" % "@VERSION@" withSources() withJavadoc()

libraryDependencies += "org.bitcoins" %% "bitcoin-s-zmq" % "@VERSION@" withSources() withJavadoc()
```

### Mill

TODO
