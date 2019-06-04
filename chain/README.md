### chain

This is meant to be a stand alone project that process a new block / transaction and stores it. 
It also provides a interface to query information related to a blockchain.

The design goal with this project is to be agnostic of how the project is receiving
the blockchain data, just that it processes and stores it. For instance
you could provide the blockchain data via

- rpc
- zmq  
- p2p
- sattelite

This project just stores relevant [`block`](../core/src/main/scala/org/bitcoins/core/protocol/blockchain/Block.scala) 
and [`transaction`](../core/src/main/scala/org/bitcoins/core/protocol/transaction/Transaction.scala) information and allows
for it to be queried via a api. 