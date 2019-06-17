# Bitcoin-S SPV node

This module is a Bitcoin SPV (simplified payment verification) node that peers
with a Bitcoin Core node over the P2P network. It syncs block headers and does
as much verification as possible with the data it has available.

The node supports bloom filters, and provides optional callbacks that notify
consumers on events such as new blocks, filtered merkle blocks and transactions.

## Caveats:

1. This is a **heavy** work in progress, and should not be used for anything serious
   yet
2. The node can only peer with one node on the P2P network right now, and that
   node must be passed in on startup. Eventually we want to support peer discovery
   through DNS seeds, as well as supporting multiple peers at the same time.
3. The majority of the P2P code was written in late 2017, and as a consequence does
   not handle some of the newer P2P messages and functionality (including SegWit
   related messages).

## Interesting files

Currently this project is a _heavy_ WIP. The most important files are

- [`Client`](src/main/scala/org/bitcoins/node/networking/Client.scala) - this handles
  all of the networking code. Currently this uses Akka but the plan is to move away
  from Akka in the future and use a networking library with a smaller classpath footprint.
- [`PeerMessageReceiver`](src/main/scala/org/bitcoins/node/networking/peer/PeerMessageReceiver.scala) - this handles messages we receive on the P2P network.
  All messages are algebraic data types, so we can easily pattern match on them and
  implement features in `PeerMessageReceiver.handleControlPayload` and
  `PeerMessageReceiver.handleDataPayload`
- [`PeerMessageReceiverState`](src/main/scala/org/bitcoins/node/networking/peer/PeerMessageReceiverState.scala) - the states that our peer message receiver can be in.
  It transitions through these states during the connect/disconnect process with our peer.
- [`PeerMessageSender`](src/main/scala/org/bitcoins/node/networking/peer/PeerMessageSender.scala) - this handles sending messages to our peer on the P2P network.
  Since we are a light client, we probably won't be sending a lot of messages to peers
  so this isn't that interesting.
- [`PeerHandler`](src/main/scala/org/bitcoins/node/networking/peer/PeerHandler.scala) - this combines a `PeerMessageReceiver` and a `PeerMessageSender` into a pair.
- [`Peer`](src/main/scala/org/bitcoins/node/models/Peer.scala) - The low level socket
  details need to connect to a peer

## Interesting tests

There is still a lot of code commented out on the project, but the tests should
pass for the ones that are not. Interesting tests are

- [`ClientTest`](../node-test/src/test/scala/org/bitcoins/node/networking/ClientTest.scala) - currently tests that we can connect with peers
- [`PeerMessageHandlerTest`](../node-test/src/test/scala/org/bitcoins/node/networking/peer/PeerMessageHandlerTest.scala) - tests that we can get our node into the
  [`PeerMessageReceiverState.Normal`](src/main/scala/org/bitcoins/node/networking/peer/PeerMessageReceiverState.scala)
  state. This means we can send/receive arbitrary messages from our peer.
- [`SpvNodeTest`] - tests that we can peer with a `bitcoind` and sync a block header

## Main method

There's a main method available in
[`SpvNodeMain.scala`](src/main/scala/org/bitcoins/node/SpvNodeMain.scala). Currently
(June 17th, 2019) the node peers with a locally running `bitcoind`. It does not do
much interesting beyond that, although you can make it more interesting if you
modify the logging levels (look in
[common-logback.xml](../core/src/test/resources/common-logback.xml)) and pass in
some callbacks to the node on startup.
