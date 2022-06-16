---
id: wallet-sports-betting-example
title: Wallet Sports Betting Example
---

This is a developer example to show to how build a
DLC with `bitcoin-s-cli` utility using the oracle
we built in our [oracle election example](../oracle/oracle-sports-betting-example.md)

This example will show you to collaborate with your peer
to create the funding transaction for your DLC.

The last thing we will do is close the DLC by broadcasting
the contract execution transaction (CET) that is valid after
the oracle broadcasts its attestations.

## Requirements for example

You need to have a fully built DLC wallet.
You can follow [this guide](../applications/server.md#building-the-server) to do this.

You will also need a the `bitcoin-s-cli` command line tool to interact with the server.
You can find how to build this [here](../applications/cli.md)

1. [Connect your server to bitcoind](../getting-setup.md#bitcoind-backend)
2. [Do intial block download (IBD) with blockfilters](../getting-setup.md#neutrino-node). This can take a few hours.

```
./app/server/target/universal/stage/bin/bitcoin-s-server
```

## 2022 Boxing Championship
In 2022 Rocky and Drago compete in the boxing championship.
People want to do a DLC based on the outcome.

### Setting up the election bet

#### Oracle
The first thing you need to create a DLC is an oracle that 
is attesting to the real world event that you are interested in.

In this case, we will be using the oracle we setup in our
[oracle election example](../oracle/oracle-sports-betting-example.md). 

The announcement that this oracle produced is 

```
fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e67
```

This announcement contains all the cryptographic information
necessary for you and your counterparty to build a DLC.

#### Contract

The next step for building a DLC is agreeing to a [contract info](https://github.com/discreetlogcontracts/dlcspecs/blob/master/Messaging.md#the-contract_info-type).
A contract info contains information about

1. The oracles that will be used by the DLC
2. The contract payout conditions based on the oracle.

Up until this point all information in this example does
NOT contain any information specific to the bitcoin network. 

If the oracle has published their announcement to the 
Suredbits oracle explorer, you can build this 
[via the contract explorer](https://test.oracle.suredbits.com/event/2ba70ec1694484e00e4adce3cf7290922a16a848664549f52eb6e80dcfdd38e6).

[Here](https://test.oracle.suredbits.com/contract/enum/f3650e03487941be8d3285f3eecd3689cbb9c4b49d1c6d467f92399647c45703) 
is a completed example of what we are going to build via the `bitcoin-s-cli`

Alice has decide that she wants to do a 100,000 sats bet. 
The amount of collateral Alice is going to contribute to the bet
is `60,000` sats.

Bob, Alice's counterparty, has agreed to contribute
`40,000` sats in collateral to the bet.

The next step is to create a `contractinfo` locally that represents
this bet. We can do this with the `createcontractinfo` rpc

```
./bitcoin-s-cli createcontractinfo fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e67 \ 100000 \ 
"{\"outcomes\" : { \"Drago_win\" : 0, \"Rocky_win\" : 100000, \"other\" : 60000 }}" 

fdd82efd010a00000000000186a0fda710330309447261676f5f77696e000000000000000009526f636b795f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e67
```

We can decode the encoded contract info (`fdd82efd010...`) with the `decodecontractinfo` to see what this represents

```
 ./bitcoin-s-cli decodecontractinfo fdd82efd010a00000000000186a0fda710330309447261676f5f77696e000000000000000009526f636b795f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e67
chmod: jre/bin/java: No such file or directory
SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder".
SLF4J: Defaulting to no-operation (NOP) logger implementation
SLF4J: See http://www.slf4j.org/codes.html#StaticLoggerBinder for further details.
{
  "totalCollateral": 100000,
  "contractDescriptor": {
    "outcomes": {
      "Drago_win": 0,
      "Rocky_win": 100000,
      "other": 60000
    },
    "hex": "fda710330309447261676f5f77696e000000000000000009526f636b795f77696e00000000000186a0056f74686572000000000000ea60"
  },
  "oracleInfo": {
    "announcement": {
      "announcementSignature": "58412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5e",
      "publicKey": "b923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cb",
      "event": {
        "nonces": [
          "0a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be"
        ],
        "maturity": "2022-06-16T00:00:00Z",
        "descriptor": {
          "outcomes": [
            "Rocky_win",
            "Drago_win",
            "other"
          ],
          "hex": "fdd8061c000309526f636b795f77696e09447261676f5f77696e056f74686572"
        },
        "eventId": "2022-championship-boxing"
      },
      "hex": "fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e67"
    }
  }
}
```

### Building funding tx/dlcs with your counterparty

Now that the contract terms are agreed upon, the next thing you need to do is 
begin the [negotiation protocol](https://github.com/discreetlogcontracts/dlcspecs/blob/master/Protocol.md#contract-negotiation) with your counterparty. 

#### Offer
The first thing you need to send your counterparty is an [`offer`](https://github.com/discreetlogcontracts/dlcspecs/blob/master/Protocol.md#the-offer_dlc-message). 
This message contains information about the utxos you will use to fund your side of the funding transaction. 

You can create the offer with the `createdlcoffer`. As arguments this rpc takes

1. contract info (what we build last step)
2. your collateral (the amount of money YOU are putting in the DLC)
3. fee rate (sats/vbyte)
4. refund locktime

As of this writing, the current block height is `2255810`. For the sake of this example
I'm going to pick a refund locktime 1 day in advance `refundLocktime=2255954`

Note: this RPC will fail if you don't have enough funds in your wallet to fund your collateral.

```
./bitcoin-s-cli createdlcoffer fdd82efd010a00000000000186a0fda710330309447261676f5f77696e000000000000000009526f636b795f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e67 \
60000 \
1 \
2255954 
a71a0043497fd7f826957108f4a30fd9cec3aeba79972084e90ead01ea330900000000fdd82efd010a00000000000186a0fda710330309447261676f5f77696e000000000000000009526f636b795f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e670333807008d4ee9767f47c5690581653f6afcd0a73e9c22a080386fd638e7b4c4500160014ffd12045f5dac88a5edc696f86be81bf7ac01c162c29cc111671d280000000000000ea600001fda714f4c8152acfe9a1763100de02000000000101074c58bcdb9859beab8a87313f370e1b5484d4276379b859144aea939777cb5b0100000000000000000233e1e40000000000160014a9d919beb7fa31022c8a7f5dc688f5d3418af74a809698000000000016001499ad8109a94a99a02a841bff0ca8fb9f35bc726102473044022030cd91d61ab2cd7f212d3e21d79dc9c087b9cb18d63c1f01dcd58b1fd9536de0022034d809826b8d7e359ce46a6cd756d82a38aabf7d122bc5e6cba81266267a6554012102c7162efd5fd28a916854b0f205e2e3b8a83d8f4a453a0bbabfebaea585651b5a0000000000000001fffffffd006b000000160014017f605089a9e4b384d5abc6301a1aee4d14716201d8ea60e124cb10df9579d570aebea2000000000000000100226bc300226c52
```

Yay! We have now created an offer (`a71a0043497fd...`) that we can send to our counterparty, Bob. If you would like to review
the offer before sending it to him you can use `decodeoffer`

```
./bitcoin-s-cli decodeoffer a71a0043497fd7f826957108f4a30fd9cec3aeba79972084e90ead01ea330900000000fdd82efd010a00000000000186a0fda710330309447261676f5f77696e000000000000000009526f636b795f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e670333807008d4ee9767f47c5690581653f6afcd0a73e9c22a080386fd638e7b4c4500160014ffd12045f5dac88a5edc696f86be81bf7ac01c162c29cc111671d280000000000000ea600001fda714f4c8152acfe9a1763100de02000000000101074c58bcdb9859beab8a87313f370e1b5484d4276379b859144aea939777cb5b0100000000000000000233e1e40000000000160014a9d919beb7fa31022c8a7f5dc688f5d3418af74a809698000000000016001499ad8109a94a99a02a841bff0ca8fb9f35bc726102473044022030cd91d61ab2cd7f212d3e21d79dc9c087b9cb18d63c1f01dcd58b1fd9536de0022034d809826b8d7e359ce46a6cd756d82a38aabf7d122bc5e6cba81266267a6554012102c7162efd5fd28a916854b0f205e2e3b8a83d8f4a453a0bbabfebaea585651b5a0000000000000001fffffffd006b000000160014017f605089a9e4b384d5abc6301a1aee4d14716201d8ea60e124cb10df9579d570aebea2000000000000000100226bc300226c52
{
  "contractFlags": "0",
  "chainHash": "43497fd7f826957108f4a30fd9cec3aeba79972084e90ead01ea330900000000",
  "contractInfo": {
    "totalCollateral": 100000,
    "contractDescriptor": {
      "outcomes": {
        "Drago_win": 0,
        "Rocky_win": 100000,
        "other": 60000
      },
      "hex": "fda710330309447261676f5f77696e000000000000000009526f636b795f77696e00000000000186a0056f74686572000000000000ea60"
    },
    "oracleInfo": {
      "announcement": {
        "announcementSignature": "58412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5e",
        "publicKey": "b923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cb",
        "event": {
          "nonces": [
            "0a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be"
          ],
          "maturity": "2022-06-16T00:00:00Z",
          "descriptor": {
            "outcomes": [
              "Rocky_win",
              "Drago_win",
              "other"
            ],
            "hex": "fdd8061c000309526f636b795f77696e09447261676f5f77696e056f74686572"
          },
          "eventId": "2022-championship-boxing"
        },
        "hex": "fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e67"
      }
    }
  },
  "fundingPubKey": "0333807008d4ee9767f47c5690581653f6afcd0a73e9c22a080386fd638e7b4c45",
  "payoutSPK": "160014ffd12045f5dac88a5edc696f86be81bf7ac01c16",
  "payoutSerialId": 3182298985477034496,
  "offerCollateral": 60000,
  "fundingInputs": [
    {
      "inputSerialId": "14417476854562780721",
      "prevTx": "02000000000101074c58bcdb9859beab8a87313f370e1b5484d4276379b859144aea939777cb5b0100000000000000000233e1e40000000000160014a9d919beb7fa31022c8a7f5dc688f5d3418af74a809698000000000016001499ad8109a94a99a02a841bff0ca8fb9f35bc726102473044022030cd91d61ab2cd7f212d3e21d79dc9c087b9cb18d63c1f01dcd58b1fd9536de0022034d809826b8d7e359ce46a6cd756d82a38aabf7d122bc5e6cba81266267a6554012102c7162efd5fd28a916854b0f205e2e3b8a83d8f4a453a0bbabfebaea585651b5a00000000",
      "prevTxVout": 1,
      "sequence": 4294967293,
      "maxWitnessLen": 107,
      "redeemScript": ""
    }
  ],
  "changeSPK": "160014017f605089a9e4b384d5abc6301a1aee4d147162",
  "changeSerialId": "133113890822474512",
  "fundOutputSerialId": 1.6110917199613313E19,
  "feeRatePerVb": 1,
  "cetLocktime": 2255811,
  "refundLocktime": 2255954,
  "temporaryContractId": "7e5295b080135d081dcca2778e2f1acbe45a6737fae0f928a2967e17bdd2ee54"
}
```
The final thing Alice needs to give bob is here wallet's tor address. This is used to interactively
build and verify contract execution transactions (CET). This can be done in a manual fashion, but I'm skipping
this for the sake of brevity and better UX. Please contact me if you would like manual steps with no built in networking.

Alice can retrieve her dlc host address with
```
./bitcoin-s-cli getdlchostaddress
degpvn2sgjmn47hzkkiubucl2xyr3etn6u7eppxipzlm2rvaghfpfqyd.onion:2862
```
She always needs to send this Bob.

#### Accept

Once Bob has received Alice's dlc host address and the offer, and reviews the terms of the offer with `decodeoffer`, he
can accept the offer.

```
./bitcoin-s-cli acceptdlc
a71a0043497fd7f826957108f4a30fd9cec3aeba79972084e90ead01ea330900000000fdd82efd010a00000000000186a0fda710330309447261676f5f77696e000000000000000009526f636b795f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e670333807008d4ee9767f47c5690581653f6afcd0a73e9c22a080386fd638e7b4c4500160014ffd12045f5dac88a5edc696f86be81bf7ac01c162c29cc111671d280000000000000ea600001fda714f4c8152acfe9a1763100de02000000000101074c58bcdb9859beab8a87313f370e1b5484d4276379b859144aea939777cb5b0100000000000000000233e1e40000000000160014a9d919beb7fa31022c8a7f5dc688f5d3418af74a809698000000000016001499ad8109a94a99a02a841bff0ca8fb9f35bc726102473044022030cd91d61ab2cd7f212d3e21d79dc9c087b9cb18d63c1f01dcd58b1fd9536de0022034d809826b8d7e359ce46a6cd756d82a38aabf7d122bc5e6cba81266267a6554012102c7162efd5fd28a916854b0f205e2e3b8a83d8f4a453a0bbabfebaea585651b5a0000000000000001fffffffd006b000000160014017f605089a9e4b384d5abc6301a1aee4d14716201d8ea60e124cb10df9579d570aebea2000000000000000100226bc300226c52 degpvn2sgjmn47hzkkiubucl2xyr3etn6u7eppxipzlm2rvaghfpfqyd.onion:2862
```
Currently this RPC doesn't return anything, but if you look at logs you should see your funding tx broadcast.
You can find logs at `~/.bitcoin-s/{mainnet,testnet3,regtest}/bitcoin-s.log`

```
2022-06-15T20:35:09UTC INFO [DataMessageHandler] We are synced
2022-06-15T20:35:09UTC INFO [DataMessageHandler] Processing 1 filters
2022-06-15T20:35:09UTC INFO [ChainHandler] Processed filters from height=2255833 to 2255833. Best filter.blockHash=0000000000000001c6e1b604954e2bbef182033453465c6d4148c779cc8e6039
2022-06-15T20:35:09UTC INFO [DLCWallet$DLCWalletImpl] Processing 1 block filters for 2 utxos and 73 scripts
2022-06-15T20:35:09UTC INFO [DataMessageHandler] Received block message with hash 0000000000000001c6e1b604954e2bbef182033453465c6d4148c779cc8e6039
2022-06-15T20:35:09UTC INFO [DLCWallet$DLCWalletImpl] Processing block=0000000000000001c6e1b604954e2bbef182033453465c6d4148c779cc8e6039 heightOpt=Some(2255833)
2022-06-15T20:35:09UTC INFO [DLCWallet$DLCWalletImpl] 1 txos are now confirmed!
2022-06-15T20:35:09UTC INFO [DLCWallet$DLCWalletImpl] Processing received utxos in tx e9f3e481596986f607a0d92c15406d6de88599996eca00b90696150212d79364 for 1 DLC(s)
2022-06-15T20:35:09UTC INFO [DLCWallet$DLCWalletImpl] Finished processing 1 received outputs, it took=19ms
2022-06-15T20:35:09UTC INFO [DLCWallet$DLCWalletImpl] 1 txos are now confirmed!
2022-06-15T20:35:09UTC INFO [DLCWallet$DLCWalletImpl] Finished processing 1 spent outputs, it took=4ms
2022-06-15T20:35:09UTC INFO [DLCWallet$DLCWalletImpl] Finished processing of block=0000000000000001c6e1b604954e2bbef182033453465c6d4148c779cc8e6039. It took 71ms
```

You can check the state of the DLC with `getdlcs`

```
./bitcoin-s-cli getdlcs
  {
    "state": "Confirmed",
    "dlcId": "af0fef54230ecb3bd4c38f82264a4fbf0b29b33d5cca2ccd50fee6bea65c53bd",
    "isInitiator": true,
    "lastUpdated": "2022-06-15T20:35:09.875Z",
    "temporaryContractId": "7e5295b080135d081dcca2778e2f1acbe45a6737fae0f928a2967e17bdd2ee54",
    "contractId": "97a17131d97adbfe1a6c7b5b9b6f77a60cdffeae942af991a4006b15af057d32",
    "contractInfo": "fdd82efd010a00000000000186a0fda710330309447261676f5f77696e000000000000000009526f636b795f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e67",
    "contractMaturity": 2255811,
    "contractTimeout": 2255954,
    "feeRate": 1,
    "totalCollateral": 100000,
    "localCollateral": 60000,
    "remoteCollateral": 40000,
    "fundingTxId": "e9f3e481596986f607a0d92c15406d6de88599996eca00b90696150212d79364",
    "payoutAddress": {
      "address": "tb1qllgjq304mtyg5hkud9hcd05phaavq8qkxz9ks4",
      "isExternal": false
    }
```

You can find things like the `fundingTxId`, `state`, `dlcId`, and `contractId` from this RPC call. Since we just
broadcast the funding transaction, the state of the DLC is `Broadcast`.

### Settling the DLC

Once the oracle has broadcast their attestations, a user can close out the DLC.
In this case of this example, you can find the oracle attestations [on the oracle explorer](https://test.oracle.suredbits.com/announcement/2ba70ec1694484e00e4adce3cf7290922a16a848664549f52eb6e80dcfdd38e6)

```
fdd8688518323032322d6368616d70696f6e736869702d626f78696e67b923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cb00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be7c83c64f33bacceb800f463a3a98ca56a40bad7c5e7a417e3f4e012f0f332d4c09526f636b795f77696e
```
We will need the contract id to close the DLC. This can be found using the `getdlcs` RPC.
Our contract id is `97a17131d97adbfe1a6c7b5b9b6f77a60cdffeae942af991a4006b15af057d32`

```
./bitcoin-s-cli executedlc 97a17131d97adbfe1a6c7b5b9b6f77a60cdffeae942af991a4006b15af057d32 fdd8688518323032322d6368616d70696f6e736869702d626f78696e67b923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cb00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be7c83c64f33bacceb800f463a3a98ca56a40bad7c5e7a417e3f4e012f0f332d4c09526f636b795f77696e
08d27fc60a208f778cead6922ae170338c90f20e58c576ac3fe5a9393a1f5b80
```

The thing returned is the CET's txid.

You've now settled your DLC :tada: :tada:



