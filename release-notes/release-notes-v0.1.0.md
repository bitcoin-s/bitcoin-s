# Important notes

This release is the first release of Bitcoin-S that's published to Sonatype, making us available on Maven Central. This means there's no need to add custom resolvers to your build configuration. We also had to change the organization name when we publish. This used to be `org.bitcoins`, this is now `org.bitcoin-s`. That means your sbt configuration should change slightly. Previously, adding Bitcoin-S looked like this:

```sbt
libraryDependencies += "org.bitcoins" %% "bitcoin-s-core" % "0.0.4"
```

This now looks like:

```sbt
libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-core" % "0.1.0"
```

# Getting the JARs

The JARs are available on Maven Central, and should be automatically downloaded when you add Bitcoin-S through your favorite build tools. You can also combine them yourself:

```bash
$ sbt publishLocal # places them in $HOME/.ivy2/local/org/bitcoin-s
$ sbt publishM2 # places them in $HOME/.m2/repository/org/bitcoin-s
```

# Highlights

- New website - Bitcoin-S has a brand new website now! It's available at https://bitcoin-s.org. It currently contains a user showcase of people using Bitcoin-S in production, some guides to using functionality in the `core` module, some guides to using the `bitcoind` RPC client, a contributing guide and the Scaladocs for Bitcoin-S. The website is a work in progress, you can expect more information to be made available there in the coming weeks and months. (https://github.com/bitcoin-s/bitcoin-s/pull/465)
- Our `bitcoind` RPC client is updated to support the 0.17 series (https://github.com/bitcoin-s/bitcoin-s/pull/384). This is of course very late, seeing as 0.18 is now released. This will hopefully take shorter time to implement than our 0.17 client :slightly_smiling_face:.
- Mnemonic codes and BIP39 seed support (https://github.com/bitcoin-s/bitcoin-s/pull/353)
- BIP84, 44 and 32 (HD derivation path standards for legacy, SegWit and SegWit-in-P2SH addresses) - Combined with support for mnemonic codes and BIP39 seeds these BIPs are crucial for implementing a wallet with Bitcoin-S (https://github.com/bitcoin-s/bitcoin-s/pull/444 and https://github.com/bitcoin-s/bitcoin-s/pull/379)
- Support for [`nBits`](https://bitcoin.org/en/glossary/nbits) compression encoding (https://github.com/bitcoin-s/bitcoin-s/pull/390)
- Separate between big endian and small endian hash types (https://github.com/bitcoin-s/bitcoin-s/pull/364) - Frustratingly, Satoshi decided that common data structures are encoded differently in the user facing RPC interfaces and the underlying Bitcoin P2P network. This is a common source of bugs, as it's often not clear what encoding we are dealing with. We now differentiate these on the type level.


# Minor changes and improvements
- Natural language syntax for currencies - https://github.com/bitcoin-s/bitcoin-s/pull/440
- Our Bech32 implementation was reworked, adding support for RegTest addresses from Bitcoin Core (https://github.com/bitcoin-s/bitcoin-s/pull/360)
- We now support the new `bitcoin.conf` format with network prefixes and sections (https://github.com/bitcoin-s/bitcoin-s/pull/478)
- It's now possible to use the `bitcoind` RPC with zero configuration, assuming you have a `bitcoind` instance running. `BitcoindRpcClient.fromDataDir()` is all you need! :tada: (https://github.com/bitcoin-s/bitcoin-s/pull/478)
- Utils for AES encryption and decryption (https://github.com/bitcoin-s/bitcoin-s/pull/395)
- [Bloop](https://github.com/scalacenter/bloop) support was added (https://github.com/bitcoin-s/bitcoin-s/pull/412), enabling a better dev experience

# `node` branch

There's also been a lot of working going into the `node` branch. This is our feature branch for three new upcoming modules in Bitcoin-S. A wallet, a SPV node and a chain syncing module. These features are not yet released, but we will have exciting news to share about this in the coming weeks.

# Thanks
Thanks to @Christewart, @torkelrogstad , @nkohen  @piu130 and @floreslorca for contributing to this release.
