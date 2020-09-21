---
id: version-v0.4-configuration
title: Application Configuration
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

## Database Migrations

All of our modules that require databases now have database migrations. The tool we use for these migrations is 
called [flyway](https://flywaydb.org/). To find your projects migraitons, you need to look inside of the 
`[project-name]/src/main/resources/[database-name]/migration/`. For example, the chain projects migrations live under 
the path `chain/src/main/resources/chaindb/migration/V1__chain_db_baseline.sql`.

Migrations can be executed by calling the [`DbManagement.migrate()`](https://github.com/bitcoin-s/bitcoin-s/blob/e387d075b0ff2e0a0fec15788fcb48e4ddc4d9d5/db-commons/src/main/scala/org/bitcoins/db/DbManagement.scala#L92) 
method. Migrations are applied by default on server startup, via the [`AppConfig.initialize()`](https://github.com/bitcoin-s/bitcoin-s/blob/master/db-commons/src/main/scala/org/bitcoins/db/AppConfig.scala#L49) 
method. 

These migrations are setup so that project's databases and migrations are independent of each other. Therefore if you
want to use the `bitcoin-s-chain` project, but not the `bitcoin-s-wallet` project, wallet migrations are not applied. 
It should be noted if you are using a module as a library, you are responsible for configuring the database via 
[slick's configuration](https://scala-slick.org/doc/3.3.1/database.html#using-typesafe-config) and calling 
[`AppConfig.initialize()`](https://github.com/bitcoin-s/bitcoin-s/blob/master/db-commons/src/main/scala/org/bitcoins/db/AppConfig.scala#L49) 
to ensure the entire module is initialized correctly.

## Example Configuration File
```$xslt
bitcoin-s {
    datadir = ${HOME}/.bitcoin-s
    network = regtest # regtest, testnet3, mainnet

    logging {
        level = WARN # trace, debug, info, warn, error, off

        # You can also tune specific module loggers.
        # They each take the same levels as above.
        # If they are commented out (as they are
        # by default), `logging.level` gets used
        # instead. 
        # The available loggers are: 

        # incoming and outgoing P2P messages
        # p2p = info

        # verification of block headers, merkle trees
        # chain-verification = info

        # generation of addresses, signing of TXs
        # key-handling = info

        # wallet operations not related to key management
        # wallet = info

        # HTTP RPC server
        # http = info

        # Database interactions
        # database = info

        # whether or not to write to the log file
        disable-file = false

        # whether or not to log to stdout 
        disable-console = false
    }

    node {
        mode = neutrino # neutrino, spv

        peers = [] # a list of peer addresses in form "hostname:portnumber"
        # (e.g. "neutrino.testnet3.suredbits.com:18333")
        # Port number is optional, the default value is 8333 for mainnet,
        # 18333 for testnet and 18444 for regtest.
    }

    chain {
        neutrino {
            filter-header-batch-size = 2000
            filter-batch-size = 100
        }
    }

    # settings for wallet module
    wallet {
        defaultAccountType = legacy # legacy, segwit, nested-segwit

        bloomFalsePositiveRate = 0.0001 # percentage

        addressGapLimit = 20

        discoveryBatchSize = 100
    }
}


akka {
    loglevel = "OFF"
    stdout-loglevel = "OFF"
    http {
        client {
            # The time after which an idle connection will be automatically closed.
            # Set to `infinite` to completely disable idle connection timeouts.

            # some requests potentially take a long time, like generate and prune
            idle-timeout = 5 minutes
        }
    }


    actor {
        debug {
            # enable DEBUG logging of all AutoReceiveMessages (Kill, PoisonPill etc.)
            autoreceive= off
            # enable function of LoggingReceive, which is to log any received message at
            # DEBUG level
            receive = on
            # enable DEBUG logging of unhandled messages
            unhandled = off

            # enable DEBUG logging of actor lifecycle changes
            lifecycle = off

            event-stream=off
        }
    }
}
```