---
id: docker-setup
title: Running Bitcoin-S using Docker on your machine
---

## Docker Setup With Bitcoin-S

### Requirements

- Ensure you have [Docker for Mac](https://docs.docker.com/docker-for-mac/install/) installed and running.
- [XQuartz](https://www.xquartz.org/) installed
    - Reboot after installing
- [bitcoin-s/bitcoin-s](https://github.com/bitcoin-s/bitcoin-s) cloned locally
- [mozilla/docker-sbt](https://github.com/mozilla/docker-sbt) cloned locally

### Running `bitcoin-s` server

1. Enter the `docker-sbt` directory and run the following:

```bash
OPENJDK_VERSION=11.0.7
docker build --build-arg OPENJDK_TAG=$OPENJDK_VERSION --tag mozilla/sbt:$OPENJDK_VERSION\_1.3.12 .
```

2. Configure `bitcoin.conf`, `bitcoin-s.conf`, `eclair.conf`

Setup `bitcoin.conf` in ~/.bitcoin

```
testnet=1
server=1
rpcuser=bitcoin
rpcpassword=local321
rpcport=18332
daemon=1
deprecatedrpc=signrawtransaction
blockfilterindex=1
debug=1
txindex=1
zmqpubrawblock=tcp://127.0.0.1:29000
zmqpubrawtx=tcp://127.0.0.1:29000
[test]
addnode=neutrino.testnet3.suredbits.com:18333
```

Setup `bitcoin-s.conf` in ~/.bitcoin-s

```
bitcoin-s {
    datadir = ${HOME}/.bitcoin-s
    network = testnet3 # regtest, testnet3, mainnet
    logging {
        level = debug # trace, debug, info, warn, error, off
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
        disable-file = falsegetfiltercount
        # whether or not to log to stdout
        disable-console = false
	logback = false
    }
    node {
        mode = neutrino # neutrino, spv
	    #peers = ["neutrino.testnet3.suredbits.com:18333"]
        #peers = ["neutrino.testnet3.suredbits.com:18333"]
	    #peers = ["localhost:18444"]
	    peers = ["localhost:18333"]
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
        defaultAccountType = segwit # legacy, segwit, nested-segwit
        bloomFalsePositiveRate = 0.0001 # percentage
        addressGapLimit = 20
        discoveryBatchSize = 100
	    requiredConfirmations = 1
    }
    eclair {
	    enabled = true
        datadir = ${HOME}/.eclair
        bitcoind {
            version = "v0.18.99" #needs to be v0.18.99 for neutrino
            datadir = ${HOME}/.bitcoin
        }
    }
}
akka {
    loglevel = "OFF"
    stdout-loglevel = "debug"
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

Setup `eclair.conf` in ~/.eclair

```
eclair {
  chain = "testnet" // "regtest" for regtest, "testnet" for testnet, "mainnet" for mainnet
  server {
    public-ips = [] // external ips, will be announced on the network
    binding-ip = "0.0.0.0"
    port = 9735
  }
  api {
    enabled = false // disabled by default for security reasons
    binding-ip = "127.0.0.1"
    port = 8080
    password = "" // password for basic auth, must be non empty if json-rpc api is enabled
  }
  enable-db-backup = true // enable the automatic sqlite db backup; do not change this unless you know what you are doing
  // override this with a script/exe that will be called everytime a new database backup has been created
  # backup-notify-script = "/absolute/path/to/script.sh"
  watcher-type = "bitcoind" // other *experimental* values include "electrum"
  bitcoind {
    host = "localhost"
    rpcport = 18332
    rpcuser = "bitcoin"
    rpcpassword = "local321"
    zmqblock = "tcp://127.0.0.1:29000"
    zmqtx = "tcp://127.0.0.1:29000"
  }
  min-feerate = 3 // minimum feerate in satoshis per byte
  smooth-feerate-window = 6 // 1 = no smoothing
  feerate-provider-timeout = 5 seconds // max time we`ll wait for answers from a fee provider before we fallback to the next one
  node-alias = "eclair"
  node-color = "49daaa"
  trampoline-payments-enable = false // TODO: @t-bast: once spec-ed this should use a global feature flag
  // see https://github.com/lightningnetwork/lightning-rfc/blob/master/09-features.md
  sync-whitelist = [] // a list of public keys; if non-empty, we will only do the initial sync with those peers
  channel-flags = 1 // announce channels
  dust-limit-satoshis = 546
  max-htlc-value-in-flight-msat = 5000000000 // 50 mBTC
  htlc-minimum-msat = 1
  max-accepted-htlcs = 30
  reserve-to-funding-ratio = 0.01 // recommended by BOLT #2
  max-reserve-to-funding-ratio = 0.05 // channel reserve can`t be more than 5% of the funding amount (recommended: 1%)
  to-remote-delay-blocks = 720 // number of blocks that the other node`s to-self outputs must be delayed (720 ~ 5 days)
  max-to-local-delay-blocks = 2016 // maximum number of blocks that we are ready to accept for our own delayed outputs (2016 ~ 2 weeks)
  mindepth-blocks = 3
  expiry-delta-blocks = 144
  // When we receive the preimage for an HTLC and want to fulfill it but the upstream peer stops responding, we want to
  // avoid letting its HTLC-timeout transaction become enforceable on-chain (otherwise there is a race condition between
  // our HTLC-success and their HTLC-timeout).
  // We will close the channel when the HTLC-timeout will happen in less than this number.
  // NB: this number effectively reduces the expiry-delta-blocks, so you may want to take that into account and increase
  // expiry-delta-blocks.
  fulfill-safety-before-timeout-blocks = 24
  fee-base-msat = 1000
  fee-proportional-millionths = 100 // fee charged per transferred satoshi in millionths of a satoshi (100 = 0.01%)
  on-chain-fees {
    default-feerates { // those are per target block, in satoshis per kilobyte
      1 = 210000
      2 = 180000
      6 = 150000
      12 = 110000
      36 = 50000
      72 = 20000
      144 = 15000
    }
    // number of blocks to target when computing fees for each transaction type
    target-blocks {
      funding = 6        // target for the funding transaction
      commitment = 2     // target for the commitment transaction (used in force-close scenario) *do not change this unless you know what you are doing*
      mutual-close = 12  // target for the mutual close transaction
      claim-main = 12    // target for the claim main transaction (tx that spends main channel output back to wallet)
    }
    feerate-tolerance {
      ratio-low = 0.5 // will allow remote fee rates as low as half our local feerate
      ratio-high = 10.0 // will allow remote fee rates as high as 10 times our local feerate
    }
    close-on-offline-feerate-mismatch = true // do not change this unless you know what you are doing
    // funder will send an UpdateFee message if the difference between current commitment fee and actual current network fee is greater
    // than this ratio.
    update-fee-min-diff-ratio = 0.1
  }
  revocation-timeout = 20 seconds // after sending a commit_sig, we will wait for at most that duration before disconnecting
  auth-timeout = 10 seconds // will disconnect if connection authentication doesn`t happen within that timeframe
  init-timeout = 10 seconds // will disconnect if initialization doesn`t happen within that timeframe
  ping-interval = 30 seconds
  ping-timeout = 10 seconds // will disconnect if peer takes longer than that to respond
  ping-disconnect = true // disconnect if no answer to our pings
  auto-reconnect = true
  initial-random-reconnect-delay = 5 seconds // we add a random delay before the first reconnection attempt, capped by this value
  max-reconnect-interval = 1 hour // max interval between two reconnection attempts, after the exponential backoff period
  payment-request-expiry = 1 hour // default expiry for payment requests generated by this node
  multi-part-payment-expiry = 60 seconds // default expiry for receiving all parts of a multi-part payment
  min-funding-satoshis = 100000
  max-funding-satoshis = 16777215 // to open channels larger than 16777215 you must enable the large_channel_support feature in 'eclair.features'
  max-payment-attempts = 5
  autoprobe-count = 0 // number of parallel tasks that send test payments to detect invalid channels
  router {
    randomize-route-selection = true // when computing a route for a payment we randomize the final selection
    channel-exclude-duration = 60 seconds // when a temporary channel failure is returned, we exclude the channel from our payment routes for this duration
    broadcast-interval = 60 seconds // see BOLT #7
    network-stats-interval = 6 hours // frequency at which we refresh global network statistics (expensive operation)
    init-timeout = 5 minutes
    sync {
      request-node-announcements = true // if true we will ask for node announcements when we receive channel ids that we don`t know
      encoding-type = zlib // encoding for short_channel_ids and timestamps in query channel sync messages; other possible value is "uncompressed"
      channel-range-chunk-size = 1500 // max number of short_channel_ids (+ timestamps + checksums) in reply_channel_range *do not change this unless you know what you are doing*
      channel-query-chunk-size = 100 // max number of short_channel_ids in query_short_channel_ids *do not change this unless you know what you are doing*
    }
    // the values below will be used to perform route searching
    path-finding {
      max-route-length = 6         // max route length for the 'first pass', if none is found then a second pass is made with no limit
      max-cltv = 1008             // max acceptable cltv expiry for the payment (1008 ~ 1 week)
      fee-threshold-sat = 21       // if fee is below this value we skip the max-fee-pct check
      max-fee-pct = 0.03           // route will be discarded if fee is above this value (in percentage relative to the total payment amount); doesn't apply if fee < fee-threshold-sat
      // channel 'weight' is computed with the following formula: channelFee * (cltvDelta * ratio-cltv + channelAge * ratio-channel-age + channelCapacity * ratio-channel-capacity)
      // the following parameters can be used to ask the router to use heuristics to find i.e: 'cltv-optimized' routes, **the sum of the three ratios must be > 0 and <= 1**
      heuristics-enable = true     // if true uses heuristics for path-finding
      ratio-cltv = 0.15            // when computing the weight for a channel, consider its CLTV delta in this proportion
      ratio-channel-age = 0.35     // when computing the weight for a channel, consider its AGE in this proportion
      ratio-channel-capacity = 0.5 // when computing the weight for a channel, consider its CAPACITY in this proportion
      mpp {
        min-amount-satoshis = 15000 // minimum amount sent via partial HTLCs
        max-parts = 6 // maximum number of HTLCs sent per payment: increasing this value will impact performance
      }
    }
  }
  socks5 {
    enabled = false
    host = "127.0.0.1"
    port = 9050
    use-for-ipv4 = true
    use-for-ipv6 = true
    use-for-tor = true
    randomize-credentials = false // this allows tor stream isolation
  }
  tor {
    enabled = false
    protocol = "v3" // v2, v3
    auth = "password" // safecookie, password
    password = "foobar" // used when auth=password
    host = "127.0.0.1"
    port = 9051
    private-key-file = "tor.dat"
  }
  db {
    driver = "sqlite" // sqlite, postgres
    postgres {
      database = "eclair"
      host = "localhost"
      port = 5432
      username = ""
      password = ""
      pool {
        max-size = 10 // recommended value = number_of_cpu_cores * 2
        connection-timeout = 30 seconds
        idle-timeout = 10 minutes
        max-life-time = 30 minutes
      }
      lease {
        interval = 5 minutes // lease-interval must be greater than lease-renew-interval
        renew-interval = 1 minute
      }
      lock-type = "lease" // lease or none
    }
  }
}
// do not edit or move this section
eclair {
  backup-mailbox {
    mailbox-type = "akka.dispatch.NonBlockingBoundedMailbox"
    mailbox-capacity = 1
  }
  backup-dispatcher {
    executor = "thread-pool-executor"
    type = PinnedDispatcher
  }
}
akka {
  io {
    tcp {
      # The maximum number of bytes delivered by a `Received` message. Before
      # more data is read from the network the connection actor will try to
      # do other work.
      # The purpose of this setting is to impose a smaller limit than the
      # configured receive buffer size. When using value 'unlimited' it will
      # try to read all from the receive buffer.
      # As per BOLT#8 lightning messages are at most 2 + 16 + 65535 + 16 = 65569bytes
      # Currently the largest message is update_add_htlc (~1500b).
      # As a tradeoff to reduce the RAM consumption, in conjunction with tcp pull mode,
      # the default value is chosen to allow for a decent number of messages to be prefetched.
      max-received-message-size = 16384b
    }
  }
}
```

3. Replace `Dockerfile` with the following:

```docker
FROM mozilla/sbt:11.0.7_1.3.12
RUN apt-get update
RUN apt-get -y install libx11-6
RUN apt-get -y install mesa-utils libgl1-mesa-glx libgtk-3-0 
WORKDIR /app
COPY . /app
RUN sbt downloadBitcoind
RUN sbt downloadEclair
RUN sbt appServer/universal:stage
RUN sbt gui/run; exit 0
WORKDIR /root
RUN wget https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-19.3.0.2/graalvm-ce-java8-linux-amd64-19.3.0.2.tar.gz
RUN tar -xzvf graalvm-ce-java8-linux-amd64-19.3.0.2.tar.gz
RUN ./graalvm-ce-java8-19.3.0.2/bin/gu install native-image
ENV OLD_JAVA_HOME="$JAVA_HOME"
ENV OLD_PATH="$PATH"
ENV JAVA_HOME=/root/graalvm-ce-java8-19.3.0.2
ENV PATH="$JAVA_HOME/bin:$PATH"
WORKDIR /app
RUN sbt cli/universal:stage
ENV JAVA_HOME="${OLD_JAVA_HOME}"
ENV PATH="${OLD_PATH}"
WORKDIR /app
RUN sbt cli/universal:stage
CMD ["/app/app/server/target/universal/stage/bin/bitcoin-s-server"]
```

4. Build the image: `docker build . -t bitcoin-s`
5. Start the server: `docker run -v $HOME/.bitcoin:/root/.bitcoin -v $HOME/.bitcoin-s/testnet3/chaindb.sqlite:/root/.bitcoin-s/testnet3/chaindb.sqlite -v $HOME/.bitcoin-s/bitcoin-s.conf:/root/.bitcoin-s/bitcoin-s.conf -v $HOME/.eclair/eclair.conf:/root/.eclair/eclair.conf -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=host.docker.internal:0 -it --name bitcoin-s bitcoin-s`

### Running the GUI

1. Open **XQuartz** and enable "Allow connections from network clients" in **Settings > Security**
2. In your terminal, allow remote connections by running  `xhost +` 
3. Run the GUI: `docker exec -it bitcoin-s sbt gui/run`
