# State of the world

Currently this project is a heavy WIP. The most important files are

- [`Client`](https://github.com/bitcoin-s/bitcoin-s-core/blob/node/node/src/main/scala/org/bitcoins/node/networking/Client.scala) - this handles all of the networking code. Currently this uses akka but the plan is to move away from akka in the future for compatability with other JVM based platforms
- [`PeerMessageReceiver`](https://github.com/bitcoin-s/bitcoin-s-core/blob/node/node/src/main/scala/org/bitcoins/node/networking/peer/PeerMessageReceiver.scala) - this handles messages we receive on the p2p network. The only messages that are currently handled are `VerackMessage` and `VersionMessage`. As this project get's built out this is where we need to add code for calling other subsystems that handle transactions, blocks, peer related information etc. All messages are algebraic data types, so we can easily pattern match on them and implement features in `PeerMessageReceiver.handleControlPayload` and `PeerMessageReceiver.handleDataPayload`
- [`PeerMessageReceiverState`](https://github.com/bitcoin-s/bitcoin-s-core/blob/node/node/src/main/scala/org/bitcoins/node/networking/peer/PeerMessageReceiverState.scala) - the states that our peer message receiver can be in. It transitions through these states during the connect/disconnect process with our peer.
- [`PeerMessageSender`](https://github.com/bitcoin-s/bitcoin-s-core/blob/node/node/src/main/scala/org/bitcoins/node/networking/peer/PeerMessageSender.scala) - this handles sending messages to our peer on the p2p network. Since we are lite client, we probably won't be sending a lot of messages to peers so this isn't that interesting.
- [`PeerHandler`](https://github.com/bitcoin-s/bitcoin-s-core/blob/node/node/src/main/scala/org/bitcoins/node/networking/peer/PeerHandler.scala) - this combines a `PeerMessageReceiver` and a `PeerMessageSender` into a pair.
- [`Peer`](https://github.com/bitcoin-s/bitcoin-s-core/blob/node/node/src/main/scala/org/bitcoins/node/models/Peer.scala) - The low level socket details need to connect to a peer


There is still a lot of code commented out on the project, but the unit tests should pass for the ones that are not. Interesting unit tests are 

- [`ClientTest`](https://github.com/bitcoin-s/bitcoin-s-core/blob/node/node-test/src/test/scala/org/bitcoins/node/networking/ClientTest.scala) - currently tests that we can connect with peers
- [`PeerMessageHandlerTest`](https://github.com/bitcoin-s/bitcoin-s-core/blob/node/node-test/src/test/scala/org/bitcoins/node/networking/peer/PeerMessageHandlerTest.scala) - tests that we can get our node into the [`PeerMessageReceiverState.Normal`](https://github.com/bitcoin-s/bitcoin-s-core/blob/node/node/src/main/scala/org/bitcoins/node/networking/peer/PeerMessageReceiverState.scala#L150) state. This means we can send / receive arbitrary messages from our peer.
