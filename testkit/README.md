[ ![Download](https://api.bintray.com/packages/bitcoin-s/bitcoin-s-core/bitcoin-s-testkit/images/download.svg) ](https://bintray.com/bitcoin-s/bitcoin-s-core/bitcoin-s-testkit/_latestVersion)

### Philosphy of testkit

The high level of of the bitcoin-s testkit is to mimic provide functionality to test 3rd party applications.

There are other examples of these in the Scala ecosystem like the `akka-testkit` and `slick-testkit`. 

We use this testkit to test bitcoin-s it self. For instance, our [BitcoindRpcClient](../bitcoind-rpc/src/main/scala/org/bitcoins/rpc/client/common/BitcoindRpcClient.scala) is tested with the functionality provided in the testkit. A quick example of a useful utility method is [BitcoindRpcTestUtil.startedBitcoindRpcClient()](src/main/scala/org/bitcoins/testkit/rpc/BitcoindRpcTestUtil.scala). This spins up a bitcoind regtest instance on machine and generates 101 blocks on that node. This gives you the abililty to start spending money immediately with that bitcoind node.

We have similar utility methods for eclair. 

### Property based testing
There is also a robust set of generators available in the [org.bitcoins.testkit.gen](src/main/scala/org/bitcoins/testkit/core/gen) package. This allows you to integrate property based testing into your library and feel confident about implementing your application specific logic correctly. 

You can see examples of us using these generators inside of testkit in our [Private Key test cases](../core-test/src/test/scala/org/bitcoins/core/crypto/ECPrivateKeySpec.scala)