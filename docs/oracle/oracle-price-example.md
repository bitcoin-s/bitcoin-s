---
id: oracle-price-example
title: Price Example
---

## Requirements for example

You need to have a fully built oracle server. You can follow [this guide](build-oracle-server.md) to do this.

You will also need a the `bitcoin-s-cli` command line tool to interact with the server.
You can find how to build this [here](../applications/cli.md)

After building the oracle server, you will need to start it with

```
./app/oracle-server/target/universal/stage/bin/bitcoin-s-oracle-server
```

## Signing BTC/USD price

BTC/USD markets trade 24/7/365 around the world. Exchanges publish the market price
for the trading pair everytime a trade is matched in their matching engine.

In this example, we will

1. Explain the `createnumericevent` and `createdigitdecompevent` rpc
2. Set up an oracle that can sign the BTC/USD price at a maturation time
3. Completing the event by signing the observed market price

For this example our maturation time will be

> 2021-02-04 00:00:00 UTC

## 2 RPC Options

When signing numbers, we need to use a [digit decomposition event](https://github.com/discreetlogcontracts/dlcspecs/blob/master/Oracle.md#version-0-digit_decomposition_event_descriptor) that can sign
each digit of a number.

There are 2 different RPC options for making a digit decomposition event: `createnumericevent` and `createdigitdecompevent`.

`createnumericevent` is meant to be user-friendly with an easy to use api, 
where `createdigitdecompevent` is meant to give to be a more advanced api that gives more expressivity. 

Here we will give examples of each that result in the same event/announcement.

### createnumericevent rpc

The `createnumericevent` takes 6 arguments

1. the name for the event (`bitcoin-s-price-example`)
2. maturation time in ISO 8601 format (`"2021-02-04T00:00:00Z"`)
3. minimum value (`0`)
4. maximum value (`131071`)
5. units (`BTC/USD`)
6. precision (`base^precision * base^numdigits` to get the actual outcome. This is useful for very small or large values)

### createdigitdecompevent rpc

It takes 6 arguments

1. the name for the event (`bitcoin-s-price-example`)
2. maturation time in seconds since the epoch (`1612396800`)
3. base (`2`)
4. number of digits (`17`)
5. units (`BTC/USD`)
6. precision (`base^precision * base^numdigits` to get the actual outcome. This is useful for very small or large values)

### Understanding createdigitdecompevent parameters

Most of these fields are self-explanatory, but one that might confuse new users
is the usage of `base` and `number of digits`. You need to set these two parameters
in such a way that your domain is contained within `base^numdigits`.

Our domain is BTC/USD price at `2020-02-04 00:00:00 UTC`. That means we need to
pick numDigits such that `2^numDigits` can contain the BTC/USD price at `2020-02-04 00:00:00 UTC`

For this example, we will pick `numDigits=17`. This means we can sign a BTC/USD price
in between `[0,2^17-1]` or `[$0-$131,071]`. If the BTC/USD price is outside of your
predetermined interval, you need to sign the min (`0`) or the max (`$131,071`).

## Setting up the BTC/USD oracle

Given the parameters we specified above, we are ready to create our digit decompisition event!

```
 ./bitcoin-s-cli createdigitdecompevent "bitcoin-s-price-example" 1612396800 2 17 "BTC/USD" 0
fdd824fd02b9659e890eef1b223ba45c9993f88c7997859302fd5510ac23f4cac0d4ee8232a77ecbdf50c07f093794370e6a506a836f6b0fb54b45f1fb662e1307166d2e57030574f77305826939fa9124d19bfa8a8b2f00f000586b8c58c79ee8b77969a949fdd822fd025300114762c188048a953803f0edeeeb68c69e6cdc1d371ba8d517003accfe05afc4d6588c3ea326512bc66c26a841adffa68330b8c723da442792e731fb19fda94274a7766bb48e520f118c100bbe62dc3806a8d05a63d92e23683a04b0b8c24148cd166585a6b33b995b3d6c083523a8435b156c05100d88f449f4754310d5574d5e88aad09af1b8ba942cfd305e728044ec6360d847254453ec05b1b518a36660e2238360e02f3a004663a7f3a3534973d8b66a2646c1386779aa820672b6361b88a8696395c0add87840b460dfd8a8c0d520017efc6bf58267d4c9d2a225c5d0e5719068a7dda5d630d7432239b6c9d921d5f3842b584503460ca52612ac2e64337d299513690372e8f4770eb8a28080e8d7c29920ca32af470d65d6f916ee81e3ac15ce02684ba6d2522a9ffea1de7e202b4b699ef7ec4f089dda07f3de5b7d1f853b2c56471999be4efca82674a651c80f047ba3a2b9e6f9999f0cd4062c533d1ae29cab2a5e33cbe98728b7b4271c67f7c5cd6e12e39128b9971e08496cbd84cfa99c77c88867d33e73acef37022ba4422a5221776991d45416db71fb54bc6c104f6a8e50e8905161709215104a7e7b97e866f32cf43233ffd615cab66699832ec607cf59c85a7f56fa957aa5f5d7ec9f46d84d5d4b777122d41ad76c6f4968aeedca243f2030d4f502e58f4181130e9afb75309ac21637bcfd0717528bfb82ffe1b6c9fadee6ba70357210990539184bcc913a0ec65837a736733a2fb6172d601b3900fdd80a11000200074254432f55534400000000001117626974636f696e2d732d70726963652d6578616d706c65
```

Yay! The hex string returned is an oracle announcement.
You can submit this on a tool like the [suredbits oracle explorer](https://oracle.suredbits.com)
so others can find your oracle.

If you are building infrastructure to automatically sign events, it is important to store two things

1. The oracle announcement above (`fdd824fd02b9659...`)
2. The timestamp that the event matures at (`1612396800`)

Now you can schedule jobs to sign the event when the maturation time passes.

## Signing the BTC/USD price for the oracle

At the maturation time (`2020-02-04 00:00:00 UTC`) you need to check the BTC/USD price.

For the purposes of this example, we are going to assume the BTC/USD price was `$42,069` BTC/USD.

Now let's sign the event. To do this, we use the `signdigits` rpc. It takes two parameters

1. The oracle announcement (`fdd824fd02b9659...`)
2. The outcome (`$42,069`)

```
./bitcoin-s-cli signdigits "bitcoin-s-price-example" 42069
fdd868fd049c17626974636f696e2d732d70726963652d6578616d706c650574f77305826939fa9124d19bfa8a8b2f00f000586b8c58c79ee8b77969a94900114762c188048a953803f0edeeeb68c69e6cdc1d371ba8d517003accfe05afc4d6c787b447aef7494301823b1570453b6a84bdadb4822cc215b057b3ef4969688f588c3ea326512bc66c26a841adffa68330b8c723da442792e731fb19fda94274f8ce423054eaccb0e3eadd81395efd9a1fe6c8c70cdb6ce5ade2515593c0306ba7766bb48e520f118c100bbe62dc3806a8d05a63d92e23683a04b0b8c24148cd2d8d787a6280d88188b85f8c1a8c2b4dc859b408e391037b27c30dd3c2fa1cbf166585a6b33b995b3d6c083523a8435b156c05100d88f449f4754310d5574d5ea403f06f5785e54e43ed84c4ecd73dcda752e0864d61bc406bd4f84495a1ce5988aad09af1b8ba942cfd305e728044ec6360d847254453ec05b1b518a36660e21e1114f3c577d8b7b954505038a9a7372b1546e04ea5c6315d3729c389faa022238360e02f3a004663a7f3a3534973d8b66a2646c1386779aa820672b6361b88b3b3e6babdf51b87edab5c22a7a017085488cb3abdabe09e780db34a8860f88ea8696395c0add87840b460dfd8a8c0d520017efc6bf58267d4c9d2a225c5d0e5ddab4419d6859eae05150160029de1065e895e434bddc6a4e0f547572dc661bb719068a7dda5d630d7432239b6c9d921d5f3842b584503460ca52612ac2e6433a694633a7914f3f155148cceb901ffb81abcb6bc244ce6d28ad6c61c8c9fdd2f7d299513690372e8f4770eb8a28080e8d7c29920ca32af470d65d6f916ee81e3bac60fa715ca02a3a801c71afd4d947c7436e05c558194ad7fdd1aeedb3e83eeac15ce02684ba6d2522a9ffea1de7e202b4b699ef7ec4f089dda07f3de5b7d1fb515a7eedb33e4d1ad111c5e2c47b36eae320d5be505293026d4c237e7eac3bf853b2c56471999be4efca82674a651c80f047ba3a2b9e6f9999f0cd4062c533dccce9f582e55e7125bb202820fa4e4ec5ffc2da14c03c3b60d53a0a990d9766b1ae29cab2a5e33cbe98728b7b4271c67f7c5cd6e12e39128b9971e08496cbd847530ec3fee05a8eeb4919569fa966b7da6960c9f2abf8d884f61c1f5f0895871cfa99c77c88867d33e73acef37022ba4422a5221776991d45416db71fb54bc6cc339855ca09a01b9a52ef520f7f328ead330803ab37155d619d8499190ed435c104f6a8e50e8905161709215104a7e7b97e866f32cf43233ffd615cab666998360d49fdec41fd2165c1c9fa8e880203034fab805b141cf22817d23ace14a0bd22ec607cf59c85a7f56fa957aa5f5d7ec9f46d84d5d4b777122d41ad76c6f49689493aba5ad6c0cb16c7d862b7cd9747418e0ffec7d3a4ec1ce394d23114bf765aeedca243f2030d4f502e58f4181130e9afb75309ac21637bcfd0717528bfb828dc8a3114da0cbb7578f57635fcd9daaf1e47cf9214b6605b9a018c1337790ccffe1b6c9fadee6ba70357210990539184bcc913a0ec65837a736733a2fb6172d581ddc2f08d27b7fd895c7fb305dddba5f3bdb923b83c3fdc3560bad5b6044a801300131013001310130013001310130013001300131013001310130013101300131
```

Yay! Now bitcoin-s gives us an attestation that is represented by the hex string `fdd868fd049...`

If you submitted your event to the [suredbits oracle explorer](https://oracle.suredbits.com) above
you will also want to submit the attestation for your event so others can find it and settle their DLCs.

If you use the `getevent` rpc along the oracle announcement, you can see the event is now completed!

```
 ./bitcoin-s-cli getevent "bitcoin-s-price-example"
{
  "nonces": [
    "4762c188048a953803f0edeeeb68c69e6cdc1d371ba8d517003accfe05afc4d6",
    "588c3ea326512bc66c26a841adffa68330b8c723da442792e731fb19fda94274",
    "a7766bb48e520f118c100bbe62dc3806a8d05a63d92e23683a04b0b8c24148cd",
    "166585a6b33b995b3d6c083523a8435b156c05100d88f449f4754310d5574d5e",
    "88aad09af1b8ba942cfd305e728044ec6360d847254453ec05b1b518a36660e2",
    "238360e02f3a004663a7f3a3534973d8b66a2646c1386779aa820672b6361b88",
    "a8696395c0add87840b460dfd8a8c0d520017efc6bf58267d4c9d2a225c5d0e5",
    "719068a7dda5d630d7432239b6c9d921d5f3842b584503460ca52612ac2e6433",
    "7d299513690372e8f4770eb8a28080e8d7c29920ca32af470d65d6f916ee81e3",
    "ac15ce02684ba6d2522a9ffea1de7e202b4b699ef7ec4f089dda07f3de5b7d1f",
    "853b2c56471999be4efca82674a651c80f047ba3a2b9e6f9999f0cd4062c533d",
    "1ae29cab2a5e33cbe98728b7b4271c67f7c5cd6e12e39128b9971e08496cbd84",
    "cfa99c77c88867d33e73acef37022ba4422a5221776991d45416db71fb54bc6c",
    "104f6a8e50e8905161709215104a7e7b97e866f32cf43233ffd615cab6669983",
    "2ec607cf59c85a7f56fa957aa5f5d7ec9f46d84d5d4b777122d41ad76c6f4968",
    "aeedca243f2030d4f502e58f4181130e9afb75309ac21637bcfd0717528bfb82",
    "ffe1b6c9fadee6ba70357210990539184bcc913a0ec65837a736733a2fb6172d"
  ],
  "eventName": "bitcoin-s-price-example",
  "signingVersion": "DLCOracleV0SigningVersion",
  "maturationTime": "2021-02-04T00:00:00Z",
  "announcementSignature": "659e890eef1b223ba45c9993f88c7997859302fd5510ac23f4cac0d4ee8232a77ecbdf50c07f093794370e6a506a836f6b0fb54b45f1fb662e1307166d2e5703",
  "eventDescriptorTLV": "fdd80a11000200074254432f555344000000000011",
  "eventTLV": "fdd822fd025300114762c188048a953803f0edeeeb68c69e6cdc1d371ba8d517003accfe05afc4d6588c3ea326512bc66c26a841adffa68330b8c723da442792e731fb19fda94274a7766bb48e520f118c100bbe62dc3806a8d05a63d92e23683a04b0b8c24148cd166585a6b33b995b3d6c083523a8435b156c05100d88f449f4754310d5574d5e88aad09af1b8ba942cfd305e728044ec6360d847254453ec05b1b518a36660e2238360e02f3a004663a7f3a3534973d8b66a2646c1386779aa820672b6361b88a8696395c0add87840b460dfd8a8c0d520017efc6bf58267d4c9d2a225c5d0e5719068a7dda5d630d7432239b6c9d921d5f3842b584503460ca52612ac2e64337d299513690372e8f4770eb8a28080e8d7c29920ca32af470d65d6f916ee81e3ac15ce02684ba6d2522a9ffea1de7e202b4b699ef7ec4f089dda07f3de5b7d1f853b2c56471999be4efca82674a651c80f047ba3a2b9e6f9999f0cd4062c533d1ae29cab2a5e33cbe98728b7b4271c67f7c5cd6e12e39128b9971e08496cbd84cfa99c77c88867d33e73acef37022ba4422a5221776991d45416db71fb54bc6c104f6a8e50e8905161709215104a7e7b97e866f32cf43233ffd615cab66699832ec607cf59c85a7f56fa957aa5f5d7ec9f46d84d5d4b777122d41ad76c6f4968aeedca243f2030d4f502e58f4181130e9afb75309ac21637bcfd0717528bfb82ffe1b6c9fadee6ba70357210990539184bcc913a0ec65837a736733a2fb6172d601b3900fdd80a11000200074254432f55534400000000001117626974636f696e2d732d70726963652d6578616d706c65",
  "announcementTLV": "fdd824fd02b9659e890eef1b223ba45c9993f88c7997859302fd5510ac23f4cac0d4ee8232a77ecbdf50c07f093794370e6a506a836f6b0fb54b45f1fb662e1307166d2e57030574f77305826939fa9124d19bfa8a8b2f00f000586b8c58c79ee8b77969a949fdd822fd025300114762c188048a953803f0edeeeb68c69e6cdc1d371ba8d517003accfe05afc4d6588c3ea326512bc66c26a841adffa68330b8c723da442792e731fb19fda94274a7766bb48e520f118c100bbe62dc3806a8d05a63d92e23683a04b0b8c24148cd166585a6b33b995b3d6c083523a8435b156c05100d88f449f4754310d5574d5e88aad09af1b8ba942cfd305e728044ec6360d847254453ec05b1b518a36660e2238360e02f3a004663a7f3a3534973d8b66a2646c1386779aa820672b6361b88a8696395c0add87840b460dfd8a8c0d520017efc6bf58267d4c9d2a225c5d0e5719068a7dda5d630d7432239b6c9d921d5f3842b584503460ca52612ac2e64337d299513690372e8f4770eb8a28080e8d7c29920ca32af470d65d6f916ee81e3ac15ce02684ba6d2522a9ffea1de7e202b4b699ef7ec4f089dda07f3de5b7d1f853b2c56471999be4efca82674a651c80f047ba3a2b9e6f9999f0cd4062c533d1ae29cab2a5e33cbe98728b7b4271c67f7c5cd6e12e39128b9971e08496cbd84cfa99c77c88867d33e73acef37022ba4422a5221776991d45416db71fb54bc6c104f6a8e50e8905161709215104a7e7b97e866f32cf43233ffd615cab66699832ec607cf59c85a7f56fa957aa5f5d7ec9f46d84d5d4b777122d41ad76c6f4968aeedca243f2030d4f502e58f4181130e9afb75309ac21637bcfd0717528bfb82ffe1b6c9fadee6ba70357210990539184bcc913a0ec65837a736733a2fb6172d601b3900fdd80a11000200074254432f55534400000000001117626974636f696e2d732d70726963652d6578616d706c65",
  "attestations": [
    "c787b447aef7494301823b1570453b6a84bdadb4822cc215b057b3ef4969688f",
    "f8ce423054eaccb0e3eadd81395efd9a1fe6c8c70cdb6ce5ade2515593c0306b",
    "2d8d787a6280d88188b85f8c1a8c2b4dc859b408e391037b27c30dd3c2fa1cbf",
    "a403f06f5785e54e43ed84c4ecd73dcda752e0864d61bc406bd4f84495a1ce59",
    "1e1114f3c577d8b7b954505038a9a7372b1546e04ea5c6315d3729c389faa022",
    "b3b3e6babdf51b87edab5c22a7a017085488cb3abdabe09e780db34a8860f88e",
    "ddab4419d6859eae05150160029de1065e895e434bddc6a4e0f547572dc661bb",
    "a694633a7914f3f155148cceb901ffb81abcb6bc244ce6d28ad6c61c8c9fdd2f",
    "bac60fa715ca02a3a801c71afd4d947c7436e05c558194ad7fdd1aeedb3e83ee",
    "b515a7eedb33e4d1ad111c5e2c47b36eae320d5be505293026d4c237e7eac3bf",
    "ccce9f582e55e7125bb202820fa4e4ec5ffc2da14c03c3b60d53a0a990d9766b",
    "7530ec3fee05a8eeb4919569fa966b7da6960c9f2abf8d884f61c1f5f0895871",
    "c339855ca09a01b9a52ef520f7f328ead330803ab37155d619d8499190ed435c",
    "60d49fdec41fd2165c1c9fa8e880203034fab805b141cf22817d23ace14a0bd2",
    "9493aba5ad6c0cb16c7d862b7cd9747418e0ffec7d3a4ec1ce394d23114bf765",
    "8dc8a3114da0cbb7578f57635fcd9daaf1e47cf9214b6605b9a018c1337790cc",
    "581ddc2f08d27b7fd895c7fb305dddba5f3bdb923b83c3fdc3560bad5b6044a8"
  ],
  "signatures": [
    "4762c188048a953803f0edeeeb68c69e6cdc1d371ba8d517003accfe05afc4d6c787b447aef7494301823b1570453b6a84bdadb4822cc215b057b3ef4969688f",
    "588c3ea326512bc66c26a841adffa68330b8c723da442792e731fb19fda94274f8ce423054eaccb0e3eadd81395efd9a1fe6c8c70cdb6ce5ade2515593c0306b",
    "a7766bb48e520f118c100bbe62dc3806a8d05a63d92e23683a04b0b8c24148cd2d8d787a6280d88188b85f8c1a8c2b4dc859b408e391037b27c30dd3c2fa1cbf",
    "166585a6b33b995b3d6c083523a8435b156c05100d88f449f4754310d5574d5ea403f06f5785e54e43ed84c4ecd73dcda752e0864d61bc406bd4f84495a1ce59",
    "88aad09af1b8ba942cfd305e728044ec6360d847254453ec05b1b518a36660e21e1114f3c577d8b7b954505038a9a7372b1546e04ea5c6315d3729c389faa022",
    "238360e02f3a004663a7f3a3534973d8b66a2646c1386779aa820672b6361b88b3b3e6babdf51b87edab5c22a7a017085488cb3abdabe09e780db34a8860f88e",
    "a8696395c0add87840b460dfd8a8c0d520017efc6bf58267d4c9d2a225c5d0e5ddab4419d6859eae05150160029de1065e895e434bddc6a4e0f547572dc661bb",
    "719068a7dda5d630d7432239b6c9d921d5f3842b584503460ca52612ac2e6433a694633a7914f3f155148cceb901ffb81abcb6bc244ce6d28ad6c61c8c9fdd2f",
    "7d299513690372e8f4770eb8a28080e8d7c29920ca32af470d65d6f916ee81e3bac60fa715ca02a3a801c71afd4d947c7436e05c558194ad7fdd1aeedb3e83ee",
    "ac15ce02684ba6d2522a9ffea1de7e202b4b699ef7ec4f089dda07f3de5b7d1fb515a7eedb33e4d1ad111c5e2c47b36eae320d5be505293026d4c237e7eac3bf",
    "853b2c56471999be4efca82674a651c80f047ba3a2b9e6f9999f0cd4062c533dccce9f582e55e7125bb202820fa4e4ec5ffc2da14c03c3b60d53a0a990d9766b",
    "1ae29cab2a5e33cbe98728b7b4271c67f7c5cd6e12e39128b9971e08496cbd847530ec3fee05a8eeb4919569fa966b7da6960c9f2abf8d884f61c1f5f0895871",
    "cfa99c77c88867d33e73acef37022ba4422a5221776991d45416db71fb54bc6cc339855ca09a01b9a52ef520f7f328ead330803ab37155d619d8499190ed435c",
    "104f6a8e50e8905161709215104a7e7b97e866f32cf43233ffd615cab666998360d49fdec41fd2165c1c9fa8e880203034fab805b141cf22817d23ace14a0bd2",
    "2ec607cf59c85a7f56fa957aa5f5d7ec9f46d84d5d4b777122d41ad76c6f49689493aba5ad6c0cb16c7d862b7cd9747418e0ffec7d3a4ec1ce394d23114bf765",
    "aeedca243f2030d4f502e58f4181130e9afb75309ac21637bcfd0717528bfb828dc8a3114da0cbb7578f57635fcd9daaf1e47cf9214b6605b9a018c1337790cc",
    "ffe1b6c9fadee6ba70357210990539184bcc913a0ec65837a736733a2fb6172d581ddc2f08d27b7fd895c7fb305dddba5f3bdb923b83c3fdc3560bad5b6044a8"
  ],
  "outcomes": [
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    [
      "0",
      "1"
    ],
    "signedOutcome": 42069
  ]
}
```
