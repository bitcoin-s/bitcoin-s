---
id: wallet-election-example
title: Wallet Election Example
---

This is a developer example to show to how build a
DLC with `bitcoin-s-cli` utility using the oracle
we built in our [oracle election example](../oracle/oracle-election-example.md)

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

Since bitcoin-s is a self custodial wallet, you will need to either

1. [Connect your server to bitcoind](../getting-setup.md#bitcoind-backend)
2. [Do intial block download (IBD) with blockfilters](../getting-setup.md#neutrino-node). This can take a few hours.

```
./app/server/target/universal/stage/bin/bitcoin-s-server
```

## US 2020 election

In 2020, the United States held a presidential election.
People want to do a DLC based on the outcome.

### Setting up the election bet

#### Oracle
The first thing you need to create a DLC is an oracle that 
is attesting to the real world event that you are interested in.

In this case, we will be using the oracle we setup in our
[oracle election example](../oracle/oracle-election-example.md). 

The announcement that this oracle produced is

```
fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e
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
[via the contract explorer](https://test.oracle.suredbits.com/event/8863cd80e1d37f668e27b84cbfed48541d671b4fed1462b86d547e7f13b5a9e4/contracts/new).

[Here](https://test.oracle.suredbits.com/contract/enum/f3650e03487941be8d3285f3eecd3689cbb9c4b49d1c6d467f92399647c45703) 
is a completed example of what we are going to build via the `bitcoin-s-cli`

Alice has decided that she wants to do a 100,000 sats bet.
The amount of collateral Alice is going to contribute to the bet
is `60,000` sats.

Bob, Alice's counterparty, has agreed to contribute
`40,000` sats in collateral to the bet.

The next step is to create a `contractinfo` locally that represents
this bet. We can do this with the `createcontractinfo` rpc

```
./bitcoin-s-cli createcontractinfo fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e \
100000 \
"{\"outcomes\" : { \"Republican_win\" : 0, \"Democrat_win\" : 100000, \"other\" : 60000 }}"                                                                                                                                                                                                        

fdd82efd011200000000000186a0fda7103b030e52657075626c6963616e5f77696e00000000000000000c44656d6f637261745f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e
```

We can decode the encoded contract info (`fdd82efd011...`) with the `decodecontractinfo` to see what this represents

```
/bitcoin-s-cli decodecontractinfo fdd82efd011200000000000186a0fda7103b030e52657075626c6963616e5f77696e00000000000000000c44656d6f637261745f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e 

{
  "totalCollateral": 100000,
  "contractDescriptor": {
    "outcomes": {
      "Republican_win": 0,
      "Democrat_win": 100000,
      "other": 60000
    }
  },
  "oracleInfo": {
    "announcement": {
      "announcementSignature": "988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5",
      "publicKey": "d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000a",
      "event": {
        "nonces": [
          "ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee14"
        ],
        "maturity": "2021-02-02T00:00:00Z",
        "descriptor": {
          "outcomes": [
            "Republican_win",
            "Democrat_win",
            "other"
          ]
        },
        "eventId": "2020-us-election"
      }
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
4. locktime
5. refund locktime

As of this writing, the current block height is `703,401`. For the sake of this example
I'm going to pick a `locktime=0` and a refund locktime 2 weeks in advance `refundLocktime=705417`

Note: this RPC will fail if you don't have enough funds in your wallet to fund your collateral.

```
./bitcoin-s-cli createdlcoffer fdd82efd011200000000000186a0fda7103b030e52657075626c6963616e5f77696e00000000000000000c44656d6f637261745f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e \
60000 \
1 \
0 \
705417
a71a006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000fdd82efd011200000000000186a0fda7103b030e52657075626c6963616e5f77696e00000000000000000c44656d6f637261745f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e02869f5d3931620521f3eef85c0e7adf64a4db330d2dfde3aa871172274f210fe0001600141df0a84b2d2e611dd595101bfed6320143c47ebbae7b7a0db8657d43000000000000ea600001fda714fd01b3875502aad4b013d8019d02000000000102b3cb67fcadc31bfbe996d12420e2843242ebcb1885c9f81327bc9a0728ad815d0000000000fdffffffb3cb67fcadc31bfbe996d12420e2843242ebcb1885c9f81327bc9a0728ad815d0100000000fdffffff039e88010000000000220020e15f5ed79f651a30dc159b015cd26b76e26c2832d660c00365ae27aa70cf8a818ddcd80000000000160014b09f423de7c54d96bdc4173689dcbe2084165d6ee1b0e602000000001600146b34fc04227d45f792e6baddfc098cc3b74a000e0247304402203aead2ad391e573d27f3f81bab703eae6712f4c3ff6985e683f551d606925e58022042fdbc5921f569f0016bd86fbd98fa2a19792606439fec818054801d59d1297e0121036cb7209a4d6ef8af38106fc109908d7ef88f80f1265a4b392adc95ccfed3362a0247304402203b55c699d340d128c124fc21834069a3e68ba2cb4bfbf7c6d8c3feb813594ee902207b11ad746d981d49d61fa8d6a40c35de7eb2fd06ab673fa8f5d4b210467080ea012102da67b76b763ac07b9574a0f9eb87cf45a71b7b95c5446ab49845771e08dce0a00000000000000001fffffffd006b000000160014eff30273b9aa3feb39fee0916a09bf2c2477df0a1c94a347be0eddf79fb7cdd1df256b830000000000000001000abf99000ac389
```

Yay! We have now created an offer (`a71a006fe...`) that we can send to our counterparty, Bob. If you would like to review
the offer before sending it to him you can use `decodeoffer`

```
./bitcoin-s-cli decodeoffer a71a006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000fdd82efd011200000000000186a0fda7103b030e52657075626c6963616e5f77696e00000000000000000c44656d6f637261745f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e02869f5d3931620521f3eef85c0e7adf64a4db330d2dfde3aa871172274f210fe0001600141df0a84b2d2e611dd595101bfed6320143c47ebbae7b7a0db8657d43000000000000ea600001fda714fd01b3875502aad4b013d8019d02000000000102b3cb67fcadc31bfbe996d12420e2843242ebcb1885c9f81327bc9a0728ad815d0000000000fdffffffb3cb67fcadc31bfbe996d12420e2843242ebcb1885c9f81327bc9a0728ad815d0100000000fdffffff039e88010000000000220020e15f5ed79f651a30dc159b015cd26b76e26c2832d660c00365ae27aa70cf8a818ddcd80000000000160014b09f423de7c54d96bdc4173689dcbe2084165d6ee1b0e602000000001600146b34fc04227d45f792e6baddfc098cc3b74a000e0247304402203aead2ad391e573d27f3f81bab703eae6712f4c3ff6985e683f551d606925e58022042fdbc5921f569f0016bd86fbd98fa2a19792606439fec818054801d59d1297e0121036cb7209a4d6ef8af38106fc109908d7ef88f80f1265a4b392adc95ccfed3362a0247304402203b55c699d340d128c124fc21834069a3e68ba2cb4bfbf7c6d8c3feb813594ee902207b11ad746d981d49d61fa8d6a40c35de7eb2fd06ab673fa8f5d4b210467080ea012102da67b76b763ac07b9574a0f9eb87cf45a71b7b95c5446ab49845771e08dce0a00000000000000001fffffffd006b000000160014eff30273b9aa3feb39fee0916a09bf2c2477df0a1c94a347be0eddf79fb7cdd1df256b830000000000000001000abf99000ac389
{
  "contractFlags": "0",
  "chainHash": "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000",
  "contractInfo": {
    "totalCollateral": 100000,
    "contractDescriptor": {
      "outcomes": {
        "Republican_win": 0,
        "Democrat_win": 100000,
        "other": 60000
      }
    },
    "oracleInfo": {
      "announcement": {
        "announcementSignature": "988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5",
        "publicKey": "d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000a",
        "event": {
          "nonces": [
            "ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee14"
          ],
          "maturity": "2021-02-02T00:00:00Z",
          "descriptor": {
            "outcomes": [
              "Republican_win",
              "Democrat_win",
              "other"
            ]
          },
          "eventId": "2020-us-election"
        }
      }
    }
  },
  "fundingPubKey": "02869f5d3931620521f3eef85c0e7adf64a4db330d2dfde3aa871172274f210fe0",
  "payoutSPK": "1600141df0a84b2d2e611dd595101bfed6320143c47ebb",
  "payoutSerialId": 1.2572776984081695E19,
  "offerCollateralSatoshis": 60000,
  "fundingInputs": [
    {
      "inputSerialId": 9.751703500876681E18,
      "prevTx": "02000000000102b3cb67fcadc31bfbe996d12420e2843242ebcb1885c9f81327bc9a0728ad815d0000000000fdffffffb3cb67fcadc31bfbe996d12420e2843242ebcb1885c9f81327bc9a0728ad815d0100000000fdffffff039e88010000000000220020e15f5ed79f651a30dc159b015cd26b76e26c2832d660c00365ae27aa70cf8a818ddcd80000000000160014b09f423de7c54d96bdc4173689dcbe2084165d6ee1b0e602000000001600146b34fc04227d45f792e6baddfc098cc3b74a000e0247304402203aead2ad391e573d27f3f81bab703eae6712f4c3ff6985e683f551d606925e58022042fdbc5921f569f0016bd86fbd98fa2a19792606439fec818054801d59d1297e0121036cb7209a4d6ef8af38106fc109908d7ef88f80f1265a4b392adc95ccfed3362a0247304402203b55c699d340d128c124fc21834069a3e68ba2cb4bfbf7c6d8c3feb813594ee902207b11ad746d981d49d61fa8d6a40c35de7eb2fd06ab673fa8f5d4b210467080ea012102da67b76b763ac07b9574a0f9eb87cf45a71b7b95c5446ab49845771e08dce0a000000000",
      "prevTxVout": 1,
      "sequence": 4294967293,
      "maxWitnessLen": 107,
      "redeemScript": null
    }
  ],
  "changeSPK": "160014eff30273b9aa3feb39fee0916a09bf2c2477df0a",
  "changeSerialId": 2059450458141810176,
  "fundOutputSerialId": 1.1508893674044221E19,
  "feeRatePerVb": 1,
  "cetLocktime": 0,
  "refundLocktime": 705417
}
```

The final thing Alice needs to give bob is here wallet's tor address. This is used to interactively
build and verify contract execution transactions (CET). This can be done in a manual fashion, but I'm skipping
this for the sake of brevity and better UX. Please contact me if you would like manual steps with no built in networking.

Alice can retrieve her dlc host address with
```
./bitcoin-s-cli getdlchostaddress
v573gl64h5zik544qvi725vxliiwwpsedarsziidp254u3cfnxlp6zqd.onion:2862
```

She always needs to send this Bob.

#### Accept

Once Bob has received Alice's dlc host address and the offer, and reviews the terms of the offer with `decodeoffer`, he
can accept the offer.

```
./bitcoin-s-cli acceptdlc a71a006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000fdd82efd011200000000000186a0fda7103b030e52657075626c6963616e5f77696e00000000000000000c44656d6f637261745f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e02869f5d3931620521f3eef85c0e7adf64a4db330d2dfde3aa871172274f210fe0001600141df0a84b2d2e611dd595101bfed6320143c47ebbae7b7a0db8657d43000000000000ea600001fda714fd01b3875502aad4b013d8019d02000000000102b3cb67fcadc31bfbe996d12420e2843242ebcb1885c9f81327bc9a0728ad815d0000000000fdffffffb3cb67fcadc31bfbe996d12420e2843242ebcb1885c9f81327bc9a0728ad815d0100000000fdffffff039e88010000000000220020e15f5ed79f651a30dc159b015cd26b76e26c2832d660c00365ae27aa70cf8a818ddcd80000000000160014b09f423de7c54d96bdc4173689dcbe2084165d6ee1b0e602000000001600146b34fc04227d45f792e6baddfc098cc3b74a000e0247304402203aead2ad391e573d27f3f81bab703eae6712f4c3ff6985e683f551d606925e58022042fdbc5921f569f0016bd86fbd98fa2a19792606439fec818054801d59d1297e0121036cb7209a4d6ef8af38106fc109908d7ef88f80f1265a4b392adc95ccfed3362a0247304402203b55c699d340d128c124fc21834069a3e68ba2cb4bfbf7c6d8c3feb813594ee902207b11ad746d981d49d61fa8d6a40c35de7eb2fd06ab673fa8f5d4b210467080ea012102da67b76b763ac07b9574a0f9eb87cf45a71b7b95c5446ab49845771e08dce0a00000000000000001fffffffd006b000000160014eff30273b9aa3feb39fee0916a09bf2c2477df0a1c94a347be0eddf79fb7cdd1df256b830000000000000001000abf99000ac389 v573gl64h5zik544qvi725vxliiwwpsedarsziidp254u3cfnxlp6zqd.onion:2862

```

Currently this RPC doesn't return anything, but if you look at logs you should see your funding tx broadcast.
You can find logs at `~/.bitcoin-s/{mainnet,testnet3,regtest}/bitcoin-s.log`

```
[info] 2021-10-03T21:02:52UTC INFO [DLCClient] connecting to SOCKS5 proxy localhost/127.0.0.1:49196
[info] 2021-10-03T21:02:52UTC INFO [DLCClient] connected to SOCKS5 proxy localhost/127.0.0.1:49196
[info] 2021-10-03T21:02:52UTC INFO [DLCClient] connecting to v573gl64h5zik544qvi725vxliiwwpsedarsziidp254u3cfnxlp6zqd.onion/<unresolved>:2862 via SOCKS5 localhost/127.0.0.1:49196
[info] 2021-10-03T21:03:02UTC INFO [DLCClient] connected to v573gl64h5zik544qvi725vxliiwwpsedarsziidp254u3cfnxlp6zqd.onion/<unresolved>:2862 via SOCKS5 proxy localhost/127.0.0.1:49196
[info] 2021-10-03T21:03:02UTC INFO [DLCDataHandler] Received LnMessage DLCOfferTLV
[info] 2021-10-03T21:03:02UTC INFO [DLCWallet$DLCWalletImpl] Creating DLC Accept for tempContractId a4e127a34eb1fc998018bfc4a7e1a6943f6588d765b1e36ca6b9c7cc67b7eaa8
[info] 2021-10-03T21:03:03UTC INFO [DLCWallet$DLCWalletImpl] Spending UTXOs: 3ef93b35f692ce8f7ee0cfeb034941dec751fd9237624482d447964cbca1b766:0
[info] 2021-10-03T21:03:03UTC INFO [DLCWallet$DLCWalletImpl] UTXO 0 details: TransactionOutput(4466999 sats,wpkh(dca26092b7540ba1c7bea286f73955a7cb247dd9))
[info] 2021-10-03T21:03:03UTC INFO [DLCWallet$DLCWalletImpl] Creating CET Sigs for a995aeca167f1c28c2b936e5bb4d87cba061dd9147217501c3523e377fd23439
[info] 2021-10-03T21:03:03UTC INFO [DLCWallet$DLCWalletImpl] Created DLCAccept for tempContractId a4e127a34eb1fc998018bfc4a7e1a6943f6588d765b1e36ca6b9c7cc67b7eaa8 with contract Id a995aeca167f1c28c2b936e5bb4d87cba061dd9147217501c3523e377fd23439
[info] 2021-10-03T21:03:04UTC INFO [DLCDataHandler] Received LnMessage DLCSignTLV
[info] 2021-10-03T21:03:04UTC INFO [DLCWallet$DLCWalletImpl] Verifying CET Signatures for contract a995aeca167f1c28c2b936e5bb4d87cba061dd9147217501c3523e377fd23439
[info] 2021-10-03T21:03:04UTC INFO [DLCWallet$DLCWalletImpl] CET Signatures are valid for contract a995aeca167f1c28c2b936e5bb4d87cba061dd9147217501c3523e377fd23439
[info] 2021-10-03T21:03:04UTC INFO [DLCWallet$DLCWalletImpl] Verifying 1 funding sigs for contract a995aeca167f1c28c2b936e5bb4d87cba061dd9147217501c3523e377fd23439
[info] 2021-10-03T21:03:05UTC INFO [DLCWallet$DLCWalletImpl] DLC a995aeca167f1c28c2b936e5bb4d87cba061dd9147217501c3523e377fd23439 sigs are verified and stored, ready to broadcast
[info] 2021-10-03T21:03:05UTC INFO [DLCWallet$DLCWalletImpl] Created funding transaction 0d74896958cee0b142a189211cac215f9f0455462290966d65ebf9fb1865de91 for contract a995aeca167f1c28c2b936e5bb4d87cba061dd9147217501c3523e377fd23439
[info] 2021-10-03T21:03:05UTC INFO [DLCWallet$DLCWalletImpl] Broadcasting funding transaction 0d74896958cee0b142a189211cac215f9f0455462290966d65ebf9fb1865de91 for contract a995aeca167f1c28c2b936e5bb4d87cba061dd9147217501c3523e377fd23439
[info] 2021-10-03T21:03:05UTC INFO [DLCWallet$DLCWalletImpl] Adding UTXO to wallet: 91de6518fbf9eb656d9690224655049f5f21ac1c2189a142b1e0ce586989740d:2 amt=4426788 sats
[info] 2021-10-03T21:03:05UTC INFO [DLCWallet$DLCWalletImpl] Successfully inserted UTXO 91de6518fbf9eb656d9690224655049f5f21ac1c2189a142b1e0ce586989740d:2 into DB
[info] 2021-10-03T21:03:05UTC INFO [DLCWallet$DLCWalletImpl] Processing tx 0d74896958cee0b142a189211cac215f9f0455462290966d65ebf9fb1865de91 for 1 DLC(s)
```

You can check the state of the DLC with `getdlcs`

```
./bitcoin-s-cli getdlcs
[
  {
    "state": "Broadcasted",
    "dlcId": "2f1224d3ab1f2456d020e7777009153e2ee5db2d127b94774580e428f5a899b9",
    "isInitiator": false,
    "lastUpdated": "2021-10-03T21:03:05.539Z",
    "tempContractId": "a4e127a34eb1fc998018bfc4a7e1a6943f6588d765b1e36ca6b9c7cc67b7eaa8",
    "contractId": "a995aeca167f1c28c2b936e5bb4d87cba061dd9147217501c3523e377fd23439",
    "contractInfo": "fdd82efd011200000000000186a0fda7103b030e52657075626c6963616e5f77696e00000000000000000c44656d6f637261745f77696e00000000000186a0056f74686572000000000000ea60fda712c7fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e",
    "contractMaturity": 704409,
    "contractTimeout": 705417,
    "feeRate": 1,
    "totalCollateral": 100000,
    "localCollateral": 40000,
    "remoteCollateral": 60000,
    "fundingTxId": "0d74896958cee0b142a189211cac215f9f0455462290966d65ebf9fb1865de91"
  }
]
```

You can find things like the `fundingTxId`, `state`, `dlcId`, and `contractId` from this RPC call. Since we just
broadcast the funding transaction, the state of the DLC is `Broadcast`.

### Settling the DLC

Once the oracle has broadcast their attestations, a user can close out the DLC.
In this case of this example, you can find the oracle
attestations [on the oracle explorer](https://test.oracle.suredbits.com/announcement/8863cd80e1d37f668e27b84cbfed48541d671b4fed1462b86d547e7f13b5a9e4)

```
fdd8688010323032302d75732d656c656374696f6ed3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000a0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1447a59ba58797e55b967aa79c89ffec67023578009c4dc1e3dee2fd75277993590c44656d6f637261745f77696e
```

We will need the contract id to close the DLC. This can be found using the `getdlcs` RPC.
Our contract id is `a995aeca167f1c28c2b936e5bb4d87cba061dd9147217501c3523e377fd23439`

```
./bitcoin-s-cli executedlc a995aeca167f1c28c2b936e5bb4d87cba061dd9147217501c3523e377fd23439  fdd8688010323032302d75732d656c656374696f6ed3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000a0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1447a59ba58797e55b967aa79c89ffec67023578009c4dc1e3dee2fd75277993590c44656d6f637261745f77696e
3cad29be34216e279d13f19153f296c4e3f8240d4a1aa04eb96a3c275478c4d1
```

The thing returned is the CET's txid.

You've now settled your DLC :tada: :tada: