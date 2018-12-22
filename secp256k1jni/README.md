[ ![Download](https://api.bintray.com/packages/bitcoin-s/bitcoin-s-core/bitcoin-s-secp256k1jni/images/download.svg) ](https://bintray.com/bitcoin-s/bitcoin-s-core/bitcoin-s-secp256k1jni/_latestVersion)

## Secp256k1jni


This project gives people a conviinent way to use [libsecp256k1](https://github.com/bitcoin-core/secp256k1) on the JVM without compiling natives themselves. 

Currently we have support for natives on

1. [linux 32 bit](natives/linux_32)
2. [linux 64 bit](natives/linux_64)
3. [mac osx 64 bit](natives/osx_64)


This uses a zero depdency library called `native-lib-loader`. The does the appropriate loading of the library onto your classpath to be accessed. To tell if you have access to libsecp256k1 you can do the following

```scala
sbt:root> project secp256k1jni
[info] Set current project to bitcoin-s-secp256k1jni (in build file:/home/chris/dev/bitcoin-s-core/)
sbt:bitcoin-s-secp256k1jni> console
[info] Starting scala interpreter...
Welcome to Scala 2.12.7 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_191).
Type in expressions for evaluation. Or try :help.

scala> import org.bitcoin.Secp256k1Context;

scala> Secp256k1Context.isEnabled()
SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder".
SLF4J: Defaulting to no-operation (NOP) logger implementation
SLF4J: See http://www.slf4j.org/codes.html#StaticLoggerBinder for further details.
res0: Boolean = true
```

## Adding secp256k1jni to your project

To add `bitcoin-s-secp256k1jni` to your project you add it like this 

```
"org.bitcoins" % "bitcoin-s-secp256k1jni" % "0.0.1"
```

or with maven

```
<dependency>
  <groupId>org.bitcoins</groupId>
  <artifactId>bitcoin-s-secp256k1jni</artifactId>
  <version>0.0.1</version>
</dependency>
```

## Using secp256k1

The file [NativeSecp256k1.java](src/main/java/org/bitcoin/NativeSecp256k1.java) contains basic functionality for 

1. Verifying digital signatures
2. Producing digital signatures
3. Computing a public key from a private key
4. tweaking keys
5. Checking public key validity
