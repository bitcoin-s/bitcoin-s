---
id: oracle-server
title: Oracle Server
---

The Oracle Server is a DLC Oracle with functionality for creating events and attesting to them.
It also provides data to be able to set up your staking address.

## Step 1: Java and Scala

To get started you will need Java, Scala, and some other nice tools installed, luckily the Scala team has an easy setup process!

Simply follow the instructions in [this short blog](https://www.scala-lang.org/2020/06/29/one-click-install.html) to get started.

## Step 2: Bitcoin-S Repository

Now, it is time to clone the [Bitcoin-S repository](https://github.com/bitcoin-s/bitcoin-s/) by running

```bashrc
git clone --recursive git@github.com:bitcoin-s/bitcoin-s.git
```

or alternatively, if you do not have ssh setup with github, you can run

```bashrc
git clone --recursive https://github.com/bitcoin-s/bitcoin-s.git
```

Next, you will want to execute the commands

```bashrc
cd bitcoin-s
git submodule update
```

to download the secp256k1 submodule, this is so cryptographic functions like signing will be faster.

## Step 3: Building the Oracle Server

You can build the oracle server with the [sbt native packager](https://github.com/sbt/sbt-native-packager).
The native packager offers [numerous ways to package the project](https://github.com/sbt/sbt-native-packager#examples).

In this example we are going to use `stage` which will produce bash scripts we can easily execute. You can stage the server with the following command.

```bash
sbt oracleServer/universal:stage
```

This will produce a script to execute bitcoin-s which you can start with

```bash
./app/oracle-server/target/universal/stage/bin/bitcoin-s-oracle-server
```

Alternatively you can run the server by just using:

```bash
sbt oracleServer/run
```

## Step 4: Configuration

If you would like to pass in a custom datadir for your server, you can do

```bash
./app/oracle-server/target/universal/stage/bin/bitcoin-s-oracle-server --datadir /path/to/datadir/
```

To use a config file that is not the `bitcoin-s.conf` file in your datadir, you can do

```bash
./app/oracle-server/target/universal/stage/bin/bitcoin-s-oracle-server --conf /path/to/file.conf
```

You can also pass in a custom `rpcport` to bind to

```bash
./app/oracle-server/target/universal/stage/bin/bitcoin-s-oracle-server --rpcport 12345
```

For more information on configuring the server please see our [configuration](../config/configuration.md) document

For more information on how to use our built in `cli` to interact with the server please see [cli.md](../applications/cli.md)

## Server Endpoints

- `getpublickey` - Get oracle's public key
- `getstakingaddress` - Get oracle's staking address
- `listevents` - Lists all oracle event TLVs
- `createevent` `label` `maturationtime` `outcomes` - Registers an oracle event
  - `label` - Label for this event
  - `maturationtime` - The earliest expected time an outcome will be signed, given in epoch second
  - `outcomes` - Possible outcomes for this event
- `createdigitdecompevent` `name` `maturationtime` `base` `numdigits` `unit` `precision` `[signed]` - Registers an oracle event that uses digit decomposition when signing the number
  - `name`- Name for this event
  - `maturationtime` - The earliest expected time an outcome will be signed, given in epoch second
  - `base` - The base in which the outcome value is decomposed
  - `numdigits` - The max number of digits the outcome can have
  - `unit` - The unit denomination of the outcome value
  - `precision` - The precision of the outcome representing the base exponent by which to multiply the number represented by the composition of the digits to obtain the actual outcome value.
  - `--signed`- Whether the outcomes can be negative
- `getevent` `event` - Get an event's details
  - `event` - The event's oracle event tlv
- `signevent` `event` `outcome` - Signs an event
    - `event` - The event's oracle event tlv
    - `outcome`- Outcome to sign for this event
- `signdigits` `event` `outcome` - Signs an event
  - `event` - The event's oracle event tlv
  - `outcome` - Number to sign for this event
- `getsignatures` `event` - Get the signatures from a signed event
  - `event` - The event's oracle event tlv
  
### Create Event Example

Bitcoin-S CLI:
```bash
$ bitcoin-s-cli createevent test 1701917137 "outcome1,outcome2,outcome3"
fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d6533

$ bitcoin-s-cli getevent fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d6533
{
  "nonces": [
    "0374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8"
  ],
  "eventName": "test",
  "signingVersion": "Mock",
  "maturationTime": "2023-12-07T02:45:37Z",
  "announcementSignature": "e27ccd54ee0e2b94c4af6e7a4ee5a73026d244dec373d6ea5d9c671d2792108c1df0b64820a1d578165cd07d37dbb4c2e6d72fbdbead156b45d8838ca1d36691",
  "eventDescriptorTLV": "fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d6533",
  "eventTLV": "fdd8226cf8d695520151bc9fbd129be6231f46b0e137b26d8ff91910c0cb6d07f6924968657131d1fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d653374657374",
  "announcementTLV": "fdd824b0e27ccd54ee0e2b94c4af6e7a4ee5a73026d244dec373d6ea5d9c671d2792108c1df0b64820a1d578165cd07d37dbb4c2e6d72fbdbead156b45d8838ca1d36691fdd8226cf8d695520151bc9fbd129be6231f46b0e137b26d8ff91910c0cb6d07f6924968657131d1fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d653374657374",
  "attestations": null,
  "signatures": null,
  "outcomes": [
    "outcome1",
    "outcome2",
    "outcome3"
  ]
}

$ bitcoin-s-cli signevent fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d6533 "outcome1"
0374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8a65977263a6b6071c29232a516adb0e69e8e049772275dc9fa8d9cfa620960dd

$ bitcoin-s-cli getsignatures fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d6533
[
  "0374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8a65977263a6b6071c29232a516adb0e69e8e049772275dc9fa8d9cfa620960dd"
]
```

CURL:
```bash
$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "createevent", "params": ["testEvent", 1701917137, ["outcome1", "outcome2", "outcome3"]]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":"fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d6533","error":null}

$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getevent", "params": ["fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d6533"]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":{"nonces":["0374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8"],"eventName":"test","signingVersion":"Mock","maturationTime":"2023-12-07T02:45:37Z","announcementSignature":"e27ccd54ee0e2b94c4af6e7a4ee5a73026d244dec373d6ea5d9c671d2792108c1df0b64820a1d578165cd07d37dbb4c2e6d72fbdbead156b45d8838ca1d36691","eventDescriptorTLV":"fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d6533","eventTLV":"fdd8226cf8d695520151bc9fbd129be6231f46b0e137b26d8ff91910c0cb6d07f6924968657131d1fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d653374657374","announcementTLV":"fdd824b0e27ccd54ee0e2b94c4af6e7a4ee5a73026d244dec373d6ea5d9c671d2792108c1df0b64820a1d578165cd07d37dbb4c2e6d72fbdbead156b45d8838ca1d36691fdd8226cf8d695520151bc9fbd129be6231f46b0e137b26d8ff91910c0cb6d07f6924968657131d1fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d653374657374","attestations":["a65977263a6b6071c29232a516adb0e69e8e049772275dc9fa8d9cfa620960dd"],"signatures":["0374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8a65977263a6b6071c29232a516adb0e69e8e049772275dc9fa8d9cfa620960dd"],"outcomes":["outcome1","outcome2","outcome3"]},"error":null}

$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "signevent", "params": ["fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d6533", "outcome 1"]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":"0374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8a65977263a6b6071c29232a516adb0e69e8e049772275dc9fa8d9cfa620960dd","error":null}

$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getsignatures", "params": ["fdd806400374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8000300086f7574636f6d653100086f7574636f6d653200086f7574636f6d6533"]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":["0374d9df0a4591e7a9ab16b091df6709220594771b7c0d5c2be6a11c4c452ef8a65977263a6b6071c29232a516adb0e69e8e049772275dc9fa8d9cfa620960dd"],"error":null}
```

### Digit Decomposition Example

Bitcoin-S CLI:

```bash
$ bitcoin-s-cli createdigitdecompevent exampleDecomp 1701917137 10 3 "units" 0 --signed
fdd80a85000a010004abb84920cb647b1dc2bbbc6b1584af8d0a1a737fe0765d7414ebffcfd9c7057d14c56615db0684b6dff24683fa25905c84a87ac9f42f75a097ceeb444ba7f851408c23c383d1b897638a8310dfd1979f3dc78c75180a5e25510cfbc2563a02557e6a8d9803662a1576e95d6165e9aa1751a909124aee60736efdde78ccef4458

$ bs-cli getevent fdd80a85000a010004abb84920cb647b1dc2bbbc6b1584af8d0a1a737fe0765d7414ebffcfd9c7057d14c56615db0684b6dff24683fa25905c84a87ac9f42f75a097ceeb444ba7f851408c23c383d1b897638a8310dfd1979f3dc78c75180a5e25510cfbc2563a02557e6a8d9803662a1576e95d6165e9aa1751a909124aee60736efdde78ccef4458
  {
    "nonces": [
      "abb84920cb647b1dc2bbbc6b1584af8d0a1a737fe0765d7414ebffcfd9c7057d",
      "14c56615db0684b6dff24683fa25905c84a87ac9f42f75a097ceeb444ba7f851",
      "408c23c383d1b897638a8310dfd1979f3dc78c75180a5e25510cfbc2563a0255",
      "7e6a8d9803662a1576e95d6165e9aa1751a909124aee60736efdde78ccef4458"
    ],
    "eventName": "exampleDecomp",
    "signingVersion": "Mock",
    "maturationTime": "2023-12-07T02:45:37Z",
    "announcementSignature": "7b149e6001496f34588ce3089df91d0f1dfbaaae988ed993c1eacf759519c358ad1fd1d98747412bf25e257c9e4cb18ece3f132fc3ef2f400258d9cf5eb9fae0",
    "eventDescriptorTLV": "fdd80a85000a010004abb84920cb647b1dc2bbbc6b1584af8d0a1a737fe0765d7414ebffcfd9c7057d14c56615db0684b6dff24683fa25905c84a87ac9f42f75a097ceeb444ba7f851408c23c383d1b897638a8310dfd1979f3dc78c75180a5e25510cfbc2563a02557e6a8d9803662a1576e95d6165e9aa1751a909124aee60736efdde78ccef4458",
    "eventTLV": "fdd822bef8d695520151bc9fbd129be6231f46b0e137b26d8ff91910c0cb6d07f6924968657131d1fdd80a85000a010004abb84920cb647b1dc2bbbc6b1584af8d0a1a737fe0765d7414ebffcfd9c7057d14c56615db0684b6dff24683fa25905c84a87ac9f42f75a097ceeb444ba7f851408c23c383d1b897638a8310dfd1979f3dc78c75180a5e25510cfbc2563a02557e6a8d9803662a1576e95d6165e9aa1751a909124aee60736efdde78ccef44586578616d706c654c6172676552616e6765",
    "announcementTLV": "fdd824fd01027b149e6001496f34588ce3089df91d0f1dfbaaae988ed993c1eacf759519c358ad1fd1d98747412bf25e257c9e4cb18ece3f132fc3ef2f400258d9cf5eb9fae0fdd822bef8d695520151bc9fbd129be6231f46b0e137b26d8ff91910c0cb6d07f6924968657131d1fdd80a85000a010004abb84920cb647b1dc2bbbc6b1584af8d0a1a737fe0765d7414ebffcfd9c7057d14c56615db0684b6dff24683fa25905c84a87ac9f42f75a097ceeb444ba7f851408c23c383d1b897638a8310dfd1979f3dc78c75180a5e25510cfbc2563a02557e6a8d9803662a1576e95d6165e9aa1751a909124aee60736efdde78ccef44586578616d706c654c6172676552616e6765",
    "attestations": null,
    "signatures": null,
    "outcomes": [
      [
        "0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9"
      ],
      [
        "0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9"
      ],
      [
        "0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9"
      ],
      [
        "+",
        "-"
      ]
    ]
  }

bitcoin-s-cli signdigits fdd80a85000a010004abb84920cb647b1dc2bbbc6b1584af8d0a1a737fe0765d7414ebffcfd9c7057d14c56615db0684b6dff24683fa25905c84a87ac9f42f75a097ceeb444ba7f851408c23c383d1b897638a8310dfd1979f3dc78c75180a5e25510cfbc2563a02557e6a8d9803662a1576e95d6165e9aa1751a909124aee60736efdde78ccef4458 123
[
  "abb84920cb647b1dc2bbbc6b1584af8d0a1a737fe0765d7414ebffcfd9c7057da391ea18b22f695bf8a34caa5a12acbdc917aea95990dbbf9568ca65676e6b7b",
  "14c56615db0684b6dff24683fa25905c84a87ac9f42f75a097ceeb444ba7f851edbab17bb86ee92834624bb7c59f490b3d03db4a4e2732eb56dde75740d5b674",
  "408c23c383d1b897638a8310dfd1979f3dc78c75180a5e25510cfbc2563a0255e4f528c5cc1a80f6aaabf2b672cea43488d277d90bfe37c9ac6f2a3cb1e7705a",
  "7e6a8d9803662a1576e95d6165e9aa1751a909124aee60736efdde78ccef4458bd578082b9e47be7d628f9c1ef43958d33f6b8b4fcae07c250f53111edc50ced"
]
```

CURL:

```
$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "createdigitdecompevent", "params": ["exampleDigitDecomp", 1701917137, 10, true, 3, "units", 0]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":"fdd80a85000a010004a9670e21aeb8f0281980657c6c6cc9e94804b9cbff650cb5a9a1b20b8556cbcd6de7afbb816d2ff9be8ff250d8075a68300e51569c96ab998fec90a6e0bd1a6cf0eb12e0c33dae1c44281eb9057910d6a40cf76987e721d622dae13e79ce160739bf4ff7c0ce8408aa23dd619c5aa5e25d078d64cd198830e8e70436f5611b1e","error":null}

$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getevent", "params": ["fdd80a85000a010004a9670e21aeb8f0281980657c6c6cc9e94804b9cbff650cb5a9a1b20b8556cbcd6de7afbb816d2ff9be8ff250d8075a68300e51569c96ab998fec90a6e0bd1a6cf0eb12e0c33dae1c44281eb9057910d6a40cf76987e721d622dae13e79ce160739bf4ff7c0ce8408aa23dd619c5aa5e25d078d64cd198830e8e70436f5611b1e"]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":{"nonces":["a9670e21aeb8f0281980657c6c6cc9e94804b9cbff650cb5a9a1b20b8556cbcd","6de7afbb816d2ff9be8ff250d8075a68300e51569c96ab998fec90a6e0bd1a6c","f0eb12e0c33dae1c44281eb9057910d6a40cf76987e721d622dae13e79ce1607","39bf4ff7c0ce8408aa23dd619c5aa5e25d078d64cd198830e8e70436f5611b1e"],"eventName":"exampleDigitDecomp","signingVersion":"Mock","maturationTime":"2023-12-07T02:45:37Z","announcementSignature":"c85357ebc6b2d3f68bc71bd0d9a3daf97b13f3911c6dc5b15141a8ca94ad610d4166c1f4f307f2ceffe33b1b081551ad5caa527d59285400b0da4b41a0ea0786","eventDescriptorTLV":"fdd80a85000a010004a9670e21aeb8f0281980657c6c6cc9e94804b9cbff650cb5a9a1b20b8556cbcd6de7afbb816d2ff9be8ff250d8075a68300e51569c96ab998fec90a6e0bd1a6cf0eb12e0c33dae1c44281eb9057910d6a40cf76987e721d622dae13e79ce160739bf4ff7c0ce8408aa23dd619c5aa5e25d078d64cd198830e8e70436f5611b1e","eventTLV":"fdd822bff8d695520151bc9fbd129be6231f46b0e137b26d8ff91910c0cb6d07f6924968657131d1fdd80a85000a010004a9670e21aeb8f0281980657c6c6cc9e94804b9cbff650cb5a9a1b20b8556cbcd6de7afbb816d2ff9be8ff250d8075a68300e51569c96ab998fec90a6e0bd1a6cf0eb12e0c33dae1c44281eb9057910d6a40cf76987e721d622dae13e79ce160739bf4ff7c0ce8408aa23dd619c5aa5e25d078d64cd198830e8e70436f5611b1e6578616d706c654c6172676552616e676531","announcementTLV":"fdd824fd0103c85357ebc6b2d3f68bc71bd0d9a3daf97b13f3911c6dc5b15141a8ca94ad610d4166c1f4f307f2ceffe33b1b081551ad5caa527d59285400b0da4b41a0ea0786fdd822bff8d695520151bc9fbd129be6231f46b0e137b26d8ff91910c0cb6d07f6924968657131d1fdd80a85000a010004a9670e21aeb8f0281980657c6c6cc9e94804b9cbff650cb5a9a1b20b8556cbcd6de7afbb816d2ff9be8ff250d8075a68300e51569c96ab998fec90a6e0bd1a6cf0eb12e0c33dae1c44281eb9057910d6a40cf76987e721d622dae13e79ce160739bf4ff7c0ce8408aa23dd619c5aa5e25d078d64cd198830e8e70436f5611b1e6578616d706c654c6172676552616e676531","attestations":null,"signatures":null,"outcomes":[["0","1","2","3","4","5","6","7","8","9"],["0","1","2","3","4","5","6","7","8","9"],["0","1","2","3","4","5","6","7","8","9"],["+","-"]]},"error":null}

$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "signdigits", "params": ["fdd80a85000a010004a9670e21aeb8f0281980657c6c6cc9e94804b9cbff650cb5a9a1b20b8556cbcd6de7afbb816d2ff9be8ff250d8075a68300e51569c96ab998fec90a6e0bd1a6cf0eb12e0c33dae1c44281eb9057910d6a40cf76987e721d622dae13e79ce160739bf4ff7c0ce8408aa23dd619c5aa5e25d078d64cd198830e8e70436f5611b1e", 123]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":["a9670e21aeb8f0281980657c6c6cc9e94804b9cbff650cb5a9a1b20b8556cbcde5d22ede02c2e6e5df6a12367082a5cba26d37d22369dd8659388738b8ed7109","6de7afbb816d2ff9be8ff250d8075a68300e51569c96ab998fec90a6e0bd1a6c43224409c57c70319b8167db086e923344f867033551a3055d7e65563db295f2","f0eb12e0c33dae1c44281eb9057910d6a40cf76987e721d622dae13e79ce160796046de397a4d2f17c01c42815005f67d801b2c0e01d1923d4bbb13559cccf4a","39bf4ff7c0ce8408aa23dd619c5aa5e25d078d64cd198830e8e70436f5611b1e0346f7853fd0d4ab792896f46714b8964875ddf0db22c2b30f4cf4ad5b0052c9"],"error":null}
```
