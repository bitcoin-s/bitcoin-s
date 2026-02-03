# Copilot Instructions for bitcoin-s

## Project Overview

Bitcoin-S is a feature-rich toolkit for building Bitcoin and Lightning applications on the JVM. The project is written in Scala and uses SBT (Scala Build Tool) as its build system. It provides modular libraries for Bitcoin protocol implementation, wallet functionality, blockchain operations, Lightning Network integration, and DLC (Discreet Log Contract) support.

## Technology Stack

- **Language**: Scala 2.13 with Scala 3 source compatibility
- **Build System**: SBT (Scala Build Tool)
- **Testing Framework**: ScalaTest 3.2.x with ScalaCheck for property-based testing
- **Code Formatting**: Scalafmt (configured via `.scalafmt.conf`)
- **JVM**: Java 25 (Zulu distribution)
- **Database**: PostgreSQL, SQLite with Slick for database access
- **Platforms**: JVM and JavaScript (using Scala.js for cross-platform modules)

## Project Structure

The project follows a modular architecture with clear separation of concerns:

- **core/**: Core Bitcoin protocol implementations
- **crypto/**: Cryptographic primitives and utilities
- **chain/**: Blockchain data structures and chain management
- **wallet/**: Wallet implementation and key management
- **node/**: Bitcoin node implementation
- **dlc-*/**: Discreet Log Contract implementations
- **bitcoind-rpc/**, **eclair-rpc/**, **lnd-rpc/**, **clightning-rpc/**: RPC client implementations
- **app/**, **app-commons/**: Application server implementations
- **testkit/**, **testkit-core/**: Testing utilities and fixtures
- **docs/**: Documentation markdown files
- **website/**: Docusaurus-based documentation website

Test modules follow the naming convention `*-test` (e.g., `core-test`, `wallet-test`).

## Building and Testing

### Build Commands

- `sbt compile` - Compile the project
- `sbt test:compile` - Compile test sources
- `sbt test` - Run all tests
- `sbt <module>/test` - Run tests for a specific module (e.g., `sbt core/test`)
- `sbt scalafmtCheckAll` - Check code formatting
- `sbt scalafmtAll` - Format all code
- `sbt +test:compile` - Cross-compile for all configured Scala versions

### Testing Approach

- **Test Framework**: ScalaTest with async test support
- **Property-Based Testing**: ScalaCheck for generating test cases
- **Test Organization**: Tests extend from base test classes like `ChainDbUnitTest`, `BitcoinSUnitTest`, etc.
- **Async Tests**: Many tests use `FutureOutcome` for async operations
- **Test Naming**: Test files end with `Test.scala` (e.g., `ChainCallbacksTest.scala`)

### Running Specific Tests

```bash
sbt "testOnly *TestClassName"
sbt "testOnly *TestClassName -- -z test name pattern"
```

## Code Style and Conventions

### Formatting

- Use Scalafmt for code formatting - always run `sbt scalafmtAll` before committing
- Configuration is in `.scalafmt.conf` with Scala 2.13 source 3 dialect
- Align openParenCallSite but not openParenDefnSite
- No dangling parentheses for call sites or definition sites

### Scala Conventions

- Use immutable data structures by default
- Prefer `Option` over `null`
- Use `Future` for asynchronous operations
- Follow functional programming principles
- Use explicit types for public APIs
- Package names follow `org.bitcoins.*` pattern

### Testing Conventions

- Test classes extend appropriate base test classes from testkit modules
- Use descriptive test names with "must" or "should" style
- Use `FutureOutcome` for async test fixtures
- Organize tests with proper setup/teardown in fixtures
- Use property-based testing (ScalaCheck) for protocol-level code

## Common Development Tasks

### Adding a New Module

Modules are defined in `build.sbt`. Each module typically has:
- A main source directory: `<module>/src/main/scala`
- A test module: `<module>-test/src/test/scala`
- Dependencies defined in `project/Deps.scala`

### Working with Databases

- Database schemas use Flyway migrations (configured in `flyway.conf`)
- Database access is through Slick
- Separate modules for SQLite and PostgreSQL support
- Test databases use in-memory configurations when possible

### Working with Documentation

- Documentation is in `docs/` directory as Markdown files
- Website uses Docusaurus (configured in `website/`)
- API documentation is generated via Scaladoc
- Use `sbt docs/mdoc` to compile documentation with code examples

### Working with RPC Clients

Each RPC client (bitcoind, eclair, lnd, c-lightning) has its own module with:
- JSON-RPC implementations
- Strongly-typed request/response models
- Async API using Futures

## Important Notes

### Memory Configuration

- JVM options are in `.jvmopts`: `-Xms512M -Xmx4G -Xss2M`
- These settings are required for building and testing the project

### CI/CD

- Multiple CI workflows for different test suites and platforms
- Tests are split across Linux, macOS, and Windows
- Separate workflows for different module groups to parallelize testing
- Docker images are built and published automatically

### Native Libraries

- `secp256k1jni` provides JNI bindings to Bitcoin's secp256k1 library
- Can be disabled with `DISABLE_SECP256K1=true` environment variable
- Native library loading uses `NativeLoader`

### Cross-Platform Modules

- Some modules (crypto, core) support both JVM and JavaScript platforms
- Use `crossProject` in build.sbt for platform-agnostic code
- Platform-specific dependencies in `.jvmSettings` and `.jsSettings`

## Troubleshooting

- If compilation is slow, check JVM memory settings in `.jvmopts`
- For test failures, ensure database migrations are up to date
- For formatting issues, run `sbt scalafmtAll`
- Clean build with `sbt clean` if encountering compilation caching issues

## Resources

- Main website: https://bitcoin-s.org
- Documentation: https://bitcoin-s.org/docs/getting-setup
- Contributing guide: https://bitcoin-s.org/docs/contributing
- GitHub repository: https://github.com/bitcoin-s/bitcoin-s
- Gitter chat: https://gitter.im/bitcoin-s-core
