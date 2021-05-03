---
id: version-0.6.0-testkit-core
title: Testkit Core
original_id: testkit-core
---

`testkit-core` is a new module that is introduced in the bitcoin-s 0.6 release.
The purpose of this module is to be scalajs compatible so that it can be used
to test the cryptoJS and coreJS modules. This means you can use this dependency
to test both your JVM and JS application that uses bitcoin-s.

If you only need to basic data structures for testing your application, you likely want
to use this module rather than `testkit`.

The testkit functionality for our core module primary consists of generators for property based tests.

A generator is a piece of code that generates a random object for a data structure -- such as a `Transaction`.

There is also a robust set of generators available in the [org.bitcoins.testkitcore.gen](../../testkit-core/src/main/scala/org/bitcoins/testkitcore/gen) package.
This allows you to integrate property based testing into your library and feel confident about implementing your application specific logic correctly.

You can see examples of us using these generators inside of testkit-core in our [Private Key test cases](../../crypto-test/src/test/scala/org/bitcoins/crypto/ECPrivateKeyTest.scala)