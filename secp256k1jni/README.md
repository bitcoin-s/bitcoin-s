[ ![Download](https://api.bintray.com/packages/bitcoin-s/bitcoin-s-core/bitcoin-s-secp256k1jni/images/download.svg) ](https://bintray.com/bitcoin-s/bitcoin-s-core/bitcoin-s-secp256k1jni/_latestVersion)

## Secp256k1jni


This project gives people a convenient way to use [libsecp256k1](https://github.com/bitcoin-core/secp256k1) on the JVM without compiling natives themselves. 

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

## Binaries

The binaries provided here are PGP signed, to give at least some assurances that
nothing funny has happened to them.

### macOS 64 bit

```bash
bitcoin-s/secp256k1jni/natives/osx_64 $ find -type f -exec sha256sum {} \; | \
        sort -k 2 | sha256sum | cut --fields 1 --delimiter=" " | \
        gpg --clearsign --output hashed-dir.sig --detach-sig

bitcoin-s/secp256k1jni/natives/osx_64 $ cat hashed-dir.sig
-----BEGIN PGP SIGNED MESSAGE-----
Hash: SHA512

341d768b915d989db82c1fb94b29faec9509ad45e30a27c217feb121747789ce
-----BEGIN PGP SIGNATURE-----

iQEzBAEBCgAdFiEEcQE9FyWyvvLjrwybHk0b2DvgrM0FAlxPHuQACgkQHk0b2Dvg
rM07EwgApXWPlVKMDP7Z8RQhhDmlnycTJxFaw315TH2IKDMEtCA0mBGY8UOy7jis
ZtQEyUQdspBnLW6RHeEBhkDlQzjMS3G4K2T2D1r2eL4vXf7dnZ2CUSy7N9Gd8Lsy
Tcdi9a/GSyFu1RCtmklCZ75/oECPlmNctdOY30+5FDIfWIcTHqKebfDstTqeYGbd
0+U5N2xDI/07g5jCF1EsgiqvFkXZEjI/44xlxWon6GtbiYR/n+MHzgwBi5XqNs2p
0t3Tvx5NbJiVVn7K2ThQFaW4ap/bqggZh1ddT/v2Wq0BH+UX6b6Abrq0tD5+ZPst
5+tz19oEa9XK+X/DDkFrT4WEMDpyIw==
=CauS
-----END PGP SIGNATURE-----

bitcoin-s/secp256k1jni/natives/osx_64 $ gpg --verify hashed-dir.sig
gpg: Signature made Mon 28 Jan 2019 04:25:24 PM CET
gpg:                using RSA key 71013D1725B2BEF2E3AF0C9B1E4D1BD83BE0ACCD
gpg: Good signature from "Torkel Rogstad <torkel@suredbits.com>" [ultimate]

```


### Linux 32 bit

TODO

### Linux 64 bit

TODO
