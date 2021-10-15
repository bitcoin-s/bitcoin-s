---
id: wallet-price-example
title: Wallet Price Example
---

This is a developer example to show to how build a
DLC with `bitcoin-s-cli` utility using the oracle
we built in our [oracle price example](../oracle/oracle-price-example.md)

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

## Betting on bitcoin

New financial markets are being built for bitcoin. Users are interested in financial
products designed to speculate on the price of bitcoin.

#### Oracle

The first thing you need to create a DLC is an oracle that
is attesting to the real world event that you are interested in.

In this case, we will be using the oracle we setup in our
[oracle election example](../oracle/oracle-price-example.md#setting-up-the-btcusd-oracle).

The announcement that this oracle produced is

```
fdd824fd02b9659e890eef1b223ba45c9993f88c7997859302fd5510ac23f4cac0d4ee8232a77ecbdf50c07f093794370e6a506a836f6b0fb54b45f1fb662e1307166d2e57030574f77305826939fa9124d19bfa8a8b2f00f000586b8c58c79ee8b77969a949fdd822fd025300114762c188048a953803f0edeeeb68c69e6cdc1d371ba8d517003accfe05afc4d6588c3ea326512bc66c26a841adffa68330b8c723da442792e731fb19fda94274a7766bb48e520f118c100bbe62dc3806a8d05a63d92e23683a04b0b8c24148cd166585a6b33b995b3d6c083523a8435b156c05100d88f449f4754310d5574d5e88aad09af1b8ba942cfd305e728044ec6360d847254453ec05b1b518a36660e2238360e02f3a004663a7f3a3534973d8b66a2646c1386779aa820672b6361b88a8696395c0add87840b460dfd8a8c0d520017efc6bf58267d4c9d2a225c5d0e5719068a7dda5d630d7432239b6c9d921d5f3842b584503460ca52612ac2e64337d299513690372e8f4770eb8a28080e8d7c29920ca32af470d65d6f916ee81e3ac15ce02684ba6d2522a9ffea1de7e202b4b699ef7ec4f089dda07f3de5b7d1f853b2c56471999be4efca82674a651c80f047ba3a2b9e6f9999f0cd4062c533d1ae29cab2a5e33cbe98728b7b4271c67f7c5cd6e12e39128b9971e08496cbd84cfa99c77c88867d33e73acef37022ba4422a5221776991d45416db71fb54bc6c104f6a8e50e8905161709215104a7e7b97e866f32cf43233ffd615cab66699832ec607cf59c85a7f56fa957aa5f5d7ec9f46d84d5d4b777122d41ad76c6f4968aeedca243f2030d4f502e58f4181130e9afb75309ac21637bcfd0717528bfb82ffe1b6c9fadee6ba70357210990539184bcc913a0ec65837a736733a2fb6172d601b3900fdd80a11000200074254432f55534400000000001117626974636f696e2d732d70726963652d6578616d706c65
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
[via the contract explorer](https://test.oracle.suredbits.com/event/362ae482860fc93bac5cbcca3f1f0e49b3c94eac92224a008bd81ef81292f43a/contracts/new).

[Here](https://test.oracle.suredbits.com/contract/numeric/d4d4df2892fb2cfd2e8f030f0e69a568e19668b5d355e7713f69853db09a4c33)
is a completed example of what we are going to build via the `bitcoin-s-cli`

Alice has decided that she wants to do a 1 bitcoin bet.
The amount of collateral Alice is going to contribute to the bet
is `50,000` sats.

Bob, Alice's counterparty, has agreed to contribute
`50,000` sats in collateral to the bet.

The next step is to create a `contractinfo` locally that represents
this bet. We can do this with the `createcontractinfo` rpc

