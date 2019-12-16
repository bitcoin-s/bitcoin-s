---
id: version-0.2.0-configuration
title: Application configuration
original_id: configuration
---

Bitcoin-S uses [HOCON](https://github.com/lightbend/config/blob/master/HOCON.md)
to configure various parts of the application the library offers. HOCON is a
superset of JSON, that is, all valid JSON is valid HOCON.

All configuration for Bitcoin-S is under the `bitcoin-s` key.

If you have a file `application.conf` anywhere on your classpath when using
bitcoin-s, the values there take precedence over the ones found in our
`reference.conf`. We also look for the file `bitcoin-s.conf` in the current
Bitcoin-S data directory.

The resolved configuration gets parsed by
[`AppConfig`](../../db-commons/src/main/scala/org/bitcoins/db/AppConfig.scala).
`AppConfig` is an abstract class that's implemented by corresponding case
classes in the `wallet`, `chain` and `node` projects. Here's some examples of how to
construct a wallet configuration:

```scala
import org.bitcoins.wallet.config.WalletAppConfig
import com.typesafe.config.ConfigFactory
import java.nio.file.Paths
import scala.util.Properties

// reads $HOME/.bitcoin-s/
val defaultConfig = WalletAppConfig.fromDefaultDatadir()


// reads a custom data directory
val customDirectory = Paths.get(Properties.userHome, "custom-bitcoin-s-directory")
val configFromCustomDatadir = WalletAppConfig(customDirectory)

// reads a custom data directory and overrides the network to be testnet3
val customOverride = ConfigFactory.parseString("bitcoin-s.network = testnet3")
val configFromCustomDirAndOverride = WalletAppConfig(customDirectory, customOverride)
```

You can pass as many `com.typesafe.config.Config`s as you'd like. If any
keys appear multiple times the last one encountered takes precedence.

## Internal configuration

Database connections are also configured by using HOCON. This is done in
[`db.conf`](../../db-commons/src/main/resources/db.conf). The options
exposed here are **not** intended to
be used by users of Bitcoin-S, and are internal only.
