# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Bitcoin-S is a Scala toolkit for building Bitcoin and Lightning applications on the JVM: protocol
primitives, a Neutrino light client node, wallet, chain/filter management, DLC (Discreet Log Contract)
support, an oracle server, and RPC clients for bitcoind/eclair/lnd/c-lightning. Package namespace is
`org.bitcoins.*`.

## Build & Test Commands

Build system is SBT. Modules follow the pattern `<module>` (main code) / `<module>-test` (tests) on
disk — e.g. `core` / `core-test`, `wallet` / `wallet-test` — but **sbt project IDs on the command line
are always the camelCase form with no hyphens**, derived from the directory name: `core-test` on disk is
`coreTest`/`coreTestJVM` as an sbt id, `wallet-test` is `walletTest`, `dlc-wallet-test` is
`dlcWalletTest`, `bitcoind-rpc-test` is `bitcoindRpcTest`, etc. A hyphenated id like `core-test/test` is
never valid sbt syntax — check `build.sbt` for the exact `lazy val <id> = ...` if unsure.

A few modules (`crypto`, `core`, `testkit-core`, `async-utils`) build for both JVM and JS via
`crossProject`, and **this applies to their test modules too**: there is no plain `coreTest` or
`cryptoTest` project you can run tasks against directly — only the JVM/JS instances exist as real sbt
projects, so it's always `coreTestJVM`/`coreTestJS`, `cryptoTestJVM`/`cryptoTestJS`,
`asyncUtilsTestJVM`/`asyncUtilsTestJS`. (`testkit-core` itself has no separate test module — it's a
fixtures library other tests depend on — so only `testkitCoreJVM`/`testkitCoreJS` exist, for compiling
it.) Every other domain/RPC/app module (`wallet`, `chain`, `node`, `dlc-wallet`, `bitcoind-rpc`, `cli`,
etc.) is a plain single-platform `project`, so `<module>Test` alone is correct for those — no JVM/JS
suffix, and no such suffix exists to add.

```bash
sbt compile                       # compile main sources
sbt Test/compile                  # compile test sources
sbt <module>Test/test             # run all tests in a single-platform module, e.g. sbt walletTest/test
sbt "<module>Test/testOnly *TestClassName"
sbt "<module>Test/testOnly *TestClassName -- -z \"test name pattern\""
sbt scalafmtCheckAll               # check formatting (CI-enforced)
sbt scalafmtAll                    # auto-format everything — run before committing
sbt coreTestJVM/test               # cross-platform module test invocation (JVM side) — NOT coreTest or core-test
sbt cryptoTestJVM/test             # same for crypto
sbt cryptoTestJS/test              # cross-platform module test invocation (JS side)
```

### Prefer the sbt thin client for repeated commands

This is a large multi-module build (8G heap per `.jvmopts`) — plain `sbt <command>` pays full JVM
startup plus build-definition loading on every invocation. When running more than one sbt command in a
session, use `sbt --client` instead so they share one persistent background sbt server:

```bash
sbt --client compile
sbt --client walletTest/test
sbt --client "coreTestJVM/testOnly *SomeSpec"
```

The first `--client` call boots the background server (slow, one-time per checkout); every subsequent
call reuses it, skipping JVM/classpath reinitialization. Restart it with `sbt --client shutdown` after
editing `build.sbt` or files under `project/` (the running server won't pick those up otherwise).

Notes:
- Tests use ScalaTest (with `FutureOutcome` for async fixtures) and ScalaCheck for property-based tests.
  Test classes extend base fixtures from `testkit`/`testkit-core` (e.g. `BitcoinSUnitTest`,
  `ChainDbUnitTest`).
- CI is split across many workflows by module group (see `.github/workflows/Linux_2.13_*.yml`,
  `Mac_2.13_*.yml`) because the full suite is too large for one job — mirror that grouping if running
  broad test sweeps locally.
- `secp256k1jni` provides native JNI bindings to libsecp256k1; native loading goes through
  `NativeLoader`. Set `DISABLE_SECP256K1=true` to fall back to pure-JVM crypto when the native lib is
  unavailable.
- Scala version is 2.13 (see `inThisBuild.sbt`) with `scala213source3` dialect for Scala 3 source
  compatibility; formatting rules live in `.scalafmt.conf`.
- `sbt docs/mdoc` compiles the Markdown docs in `docs/` with live code examples (Docusaurus site lives
  in `website/`).

## Architecture

### Module dependency layers

Dependencies flow strictly downward; higher layers depend on lower ones (never the reverse):

1. **`crypto`** — elliptic curve primitives, hashing, signature schemes (secp256k1-backed, with a
   pure-JVM/JS fallback). Depends on `secp256k1jni` on the JVM side.
2. **`core`** — Bitcoin protocol data structures (transactions, scripts, blocks, addresses, PSBT, DLC
   messages), serialization, script interpreter. No I/O, no actor system — pure protocol logic. Depends
   only on `crypto`.
3. **`async-utils`, `testkit-core`** — small cross-platform helpers/fixtures built on `core`.
4. **`app-commons`** — shared server/CLI concerns: `AppConfig`/`AppConfigFactory` (the config-loading
   base class every module-specific config extends), JSON models shared between server and CLI.
5. **`db-commons`, `key-manager`** — Slick-based DB access layer and BIP32/39 key management, each
   depending on `core` + `app-commons`.
6. **Domain modules** — `chain`, `wallet`, `node`, `dlc-oracle`, `dlc-wallet`, `dlc-node`, `fee-provider`,
   `esplora`, `tor`, `zmq`: the actual protocol/business logic, each with its own DB schema
   (Flyway-migrated, see below), its own `AppConfig` subclass, and (mostly) its own callback trait for
   observing internal events (e.g. `NodeCallbacks`, wallet callbacks).
7. **RPC clients** — `bitcoind-rpc`, `eclair-rpc`, `lnd-rpc`, `clightning-rpc`: JSON-RPC wrappers with
   strongly-typed request/response models, used both standalone and by tests as a "ground truth" Bitcoin
   node/Lightning node to test against.
8. **`app/`** — deployables: `server` (the main app server, aggregates chain+wallet+node+dlc-wallet
   etc. behind an HTTP JSON-RPC API), `oracle-server` (serves `dlc-oracle`), `cli`/`cli-grpc` (command
   line clients hitting the server's API, see `ConsoleCli.scala`), `server-grpc`, `gui`, `bundle`.
9. **`testkit`** — full-stack test fixtures (spins up real chain/wallet/node instances, bitcoind regtest,
   etc.) used by the `*-test` modules of domain packages; **`testkit-core`** is the lighter
   cross-platform subset used by `crypto`/`core` tests.

When changing a low-level module (`crypto`, `core`), assume ripple effects through the whole dependency
graph and check `build.sbt` for exact `.dependsOn(...)` edges before assuming a module is isolated.

### Node module (Neutrino light client)

`node/` implements a Neutrino (BIP157/158 compact filter) light client: `PeerManager`/`PeerFinder`
manage P2P connections, `NeutrinoNode`/`Node` drive sync state (`NodeState`), and `NodeCallbacks` notify
subscribers (e.g. the wallet) of new blocks/filters/transactions. This is the primary way bitcoin-s
tracks chain state without a full node — `bitcoind-rpc` is used instead when talking to an actual
Bitcoin Core instance (e.g. in tests, or when the user configures a full-node backend).

### Config system

Every long-running module has an `AppConfig` subclass (base in
`app-commons/src/main/scala/org/bitcoins/commons/config/AppConfig.scala`) built from HOCON
(`reference.conf` per module, e.g. `db-commons/src/main/resources/reference.conf`,
`app/server/src/main/resources/reference.conf`), user-overridable via `bitcoin-s.conf`. DB-backed
modules run Flyway migrations on startup (`flyway.conf`), with separate migration paths for SQLite vs
PostgreSQL.

### Concurrency

Built on Apache Pekko (the Akka fork — `org.apache.pekko` artifacts; code and docs still often say
"akka" from the pre-fork history). Async operations use `scala.concurrent.Future` throughout; streaming
data (peer connections, chain sync) uses Pekko Streams.

### DLC support

Discreet Log Contracts span three modules: `dlc-oracle` (attestation signing, depends on `key-manager`),
`dlc-wallet` (contract negotiation/execution, extends `wallet`), and `dlc-node` (peer-to-peer DLC message
routing, depends on `tor`). `dlc-commons` holds shared message/state types.
