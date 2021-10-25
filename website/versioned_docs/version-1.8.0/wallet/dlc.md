---
id: version-1.8.0-dlc
title: Executing A DLC with Bitcoin-S
original_id: dlc
---

## Executing A Discreet Log Contract (DLC)

## Step 1: Get Bitcoin-S Setup

See the [setup document](../getting-setup.md).

### Using the GUI

To first start up the GUI you first need to start your bitcoin-s server and gui with

```bashrc
sbt bundle/run
```

or if your bitcoin-s server is already running, you can run the standalone gui with

```bashrc
sbt gui/run
```

or by following the instructions for building and running the GUI [here](../getting-setup.md#step-5-setting-up-a-bitcoin-s-server)

## Step 2: Agree On Contract Terms

Both parties must agree on all fields from the table below:

|   Field Name   |                       Description                        |
| :------------: | :------------------------------------------------------: |
|  contractInfo  |    Information about payouts and which oracles to use    |
|   collateral   |        Number of sats the initiator is putting up        |
|    locktime    |                  Locktime of the CETs                    |
| refundlocktime |            Locktime of the Refund Transaction            |
|    feerate     |                 Fee rate in sats/vbyte                   |

> Note: if you wish to set up your own oracle for testing, you can do so by checking out our [oracle rpc server](../oracle/oracle-server.md) or [Krystal Bull](https://github.com/bitcoin-s/krystal-bull)

### Decoding DLC Messages

With the cli tool you can decode dlc offers, contract infos, oracle announcements, and oracle attestations.
This can be helpful for when developing on top of bitcoin-s and not having the proper DLC tooling to understand the messages.

Examples:

Decoding Offer:
```bash
bitcoin-s-cli decodeoffer a71a006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000fdd82ee40000000000010f2cfda7101802035965730000000000010f2c024e6f0000000000000000fda712bcfdd824b8d89fe4d84bdf96299d2e3664acda7e14001297f341afcaca91d3909f12988cb308f309625d89c078bf2268b174faf1c082f1f10bef55834af50b91d9a241ffb2b8b005b07acf849ad2cec22107331dedbf5a607654fad4eafe39c278e27dde68fdd822540001ef8bd9f4445543c63923dde8c00f60d649c445a7772ac4693136b2fd234fcc2f60dfa880fdd80609000203596573024e6f20446f657320746865206d656d706f6f6c20636c656172206f6e20372f322f323102279cf17c387d7101b08d90b2aeef46231957d9d3558f5871e62781a68e7d75c9001600148dc81a3097bf28c6020eaf89cefae69c5f31aceb5d655d867008d23e00000000000090880001fda714fd01b3627231e09b7948c3019d02000000000102368af13baeef5a313d3ef12e895b232f1e76212cb76957d7e4b8a664915d15960100000000fdfffffffaec4ef43ae2365d0abc53336188053da4078a5b7489928602bcc8c9a4b75f0d0000000000fdffffff030f0b000000000000160014107eb2463ad25843ed01fd33656c83bdd11db3554a8701000000000022002002265c41f299b3c7f0dda5b9d7bc4135d25c2d8aed286837aa9e0954d70894d606961c000000000016001482a7ecd4624e6856d1b09c27e9f3a82323f49c2d0247304402200911162e99e23e4a26e0219a4bdaaf7a5f790be63a8376516640dcc0860ca4d602203a078371904842e2e6adfbebebcac138db23514b3396851569f5f2bf82c3197a012103cd2d0a4eace5993ebb0a75da85d9070627e64f1a5783cf5217e9bd82d20d1c470247304402200fa8515323410ca2b14b08bf22c618b6ced77b9289cb1dfa7ac10548e2e1b2e002206a407bcafdfb64009182fb5838db9671968abdd902e1194f6883cd0e5413ad36012103d3864eb755690e2b4ff139047419373e36a05630429da76fef5de25eeffb4ffc0000000000000002fffffffd006b000000160014d13be5f330b1ea9bbf61f68002b4465e02c341d9df27e1460161e002825311ec94d1e91b000000000000000a0000000061316580
{
  "contractFlags": "0",
  "chainHash": "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000",
  "contractInfo": {
    "totalCollateral": 69420,
    "contractDescriptor": {
      "outcomes": [
        {
          "outcome": "Yes",
          "localPayout": 69420
        },
        {
          "outcome": "No",
          "localPayout": 0
        }
      ]
    },
    "oracleInfo": {
      "announcement": {
        "announcementSignature": "d89fe4d84bdf96299d2e3664acda7e14001297f341afcaca91d3909f12988cb308f309625d89c078bf2268b174faf1c082f1f10bef55834af50b91d9a241ffb2",
        "publicKey": "b8b005b07acf849ad2cec22107331dedbf5a607654fad4eafe39c278e27dde68",
        "event": {
          "nonces": [
            "ef8bd9f4445543c63923dde8c00f60d649c445a7772ac4693136b2fd234fcc2f"
          ],
          "maturity": "2021-07-03T00:00:00Z",
          "descriptor": {
            "outcomes": [
              "Yes",
              "No"
            ]
          },
          "eventId": "Does the mempool clear on 7/2/21"
        }
      }
    }
  },
  "fundingPubKey": "02279cf17c387d7101b08d90b2aeef46231957d9d3558f5871e62781a68e7d75c9",
  "payoutSPK": "1600148dc81a3097bf28c6020eaf89cefae69c5f31aceb",
  "payoutSerialId": 6729888050161701888,
  "offerCollateralSatoshis": 37000,
  "fundingInputs": [
    {
      "inputSerialId": 7093787203812804608,
      "prevTx": "02000000000102368af13baeef5a313d3ef12e895b232f1e76212cb76957d7e4b8a664915d15960100000000fdfffffffaec4ef43ae2365d0abc53336188053da4078a5b7489928602bcc8c9a4b75f0d0000000000fdffffff030f0b000000000000160014107eb2463ad25843ed01fd33656c83bdd11db3554a8701000000000022002002265c41f299b3c7f0dda5b9d7bc4135d25c2d8aed286837aa9e0954d70894d606961c000000000016001482a7ecd4624e6856d1b09c27e9f3a82323f49c2d0247304402200911162e99e23e4a26e0219a4bdaaf7a5f790be63a8376516640dcc0860ca4d602203a078371904842e2e6adfbebebcac138db23514b3396851569f5f2bf82c3197a012103cd2d0a4eace5993ebb0a75da85d9070627e64f1a5783cf5217e9bd82d20d1c470247304402200fa8515323410ca2b14b08bf22c618b6ced77b9289cb1dfa7ac10548e2e1b2e002206a407bcafdfb64009182fb5838db9671968abdd902e1194f6883cd0e5413ad36012103d3864eb755690e2b4ff139047419373e36a05630429da76fef5de25eeffb4ffc00000000",
      "prevTxVout": 2,
      "sequence": 4294967293,
      "maxWitnessLen": 107,
      "redeemScript": null
    }
  ],
  "changeSPK": "160014d13be5f330b1ea9bbf61f68002b4465e02c341d9",
  "changeSerialId": 1.6080068685336797E19,
  "fundOutputSerialId": 9.390869355804355E18,
  "feeRatePerVb": 10,
  "cetLocktime": 0,
  "refundLocktime": 1630627200
}
```

Decoding Contract Info:
```bash
bitcoin-s-cli decodecontractinfo fdd82ee40000000000010f2cfda7101802035965730000000000010f2c024e6f0000000000000000fda712bcfdd824b8d89fe4d84bdf96299d2e3664acda7e14001297f341afcaca91d3909f12988cb308f309625d89c078bf2268b174faf1c082f1f10bef55834af50b91d9a241ffb2b8b005b07acf849ad2cec22107331dedbf5a607654fad4eafe39c278e27dde68fdd822540001ef8bd9f4445543c63923dde8c00f60d649c445a7772ac4693136b2fd234fcc2f60dfa880fdd80609000203596573024e6f20446f657320746865206d656d706f6f6c20636c656172206f6e20372f322f3231
{
  "totalCollateral": 69420,
  "contractDescriptor": {
    "outcomes": [
      {
        "outcome": "Yes",
        "localPayout": 69420
      },
      {
        "outcome": "No",
        "localPayout": 0
      }
    ]
  },
  "oracleInfo": {
    "announcement": {
      "announcementSignature": "d89fe4d84bdf96299d2e3664acda7e14001297f341afcaca91d3909f12988cb308f309625d89c078bf2268b174faf1c082f1f10bef55834af50b91d9a241ffb2",
      "publicKey": "b8b005b07acf849ad2cec22107331dedbf5a607654fad4eafe39c278e27dde68",
      "event": {
        "nonces": [
          "ef8bd9f4445543c63923dde8c00f60d649c445a7772ac4693136b2fd234fcc2f"
        ],
        "maturity": "2021-07-03T00:00:00Z",
        "descriptor": {
          "outcomes": [
            "Yes",
            "No"
          ]
        },
        "eventId": "Does the mempool clear on 7/2/21"
      }
    }
  }
```

Decoding Oracle Announcement:
```bash
bitcoin-s-cli decodeannouncement fdd824b1585e18c3bc1d922854329fdb5a402713b161b7baebb6b6f3249936ef79c0a6671027afd7a1126d79e589811042eb4885c0cb7c150b4c4863a1e35a2ac432f7c81d5dcdba2e64cb116cc0c375a0856298f0058b778f46bfe625ac6576204889e4fdd8224d0001424c11a44c2e522f90bbe4abab6ec1bc8ab44c9b29316ce6e1d0d7d08385a474603c2e80fdd80609000203594553024e4f194254432d5553442d4f5645522d35304b2d434f494e42415345
{
  "announcementSignature": "585e18c3bc1d922854329fdb5a402713b161b7baebb6b6f3249936ef79c0a6671027afd7a1126d79e589811042eb4885c0cb7c150b4c4863a1e35a2ac432f7c8",
  "publicKey": "1d5dcdba2e64cb116cc0c375a0856298f0058b778f46bfe625ac6576204889e4",
  "event": {
    "nonces": [
      "424c11a44c2e522f90bbe4abab6ec1bc8ab44c9b29316ce6e1d0d7d08385a474"
    ],
    "maturity": "2021-03-01T00:00:00Z",
    "descriptor": {
      "outcomes": [
        "YES",
        "NO"
      ]
    },
    "eventId": "BTC-USD-OVER-50K-COINBASE"
  }
}
```

Decoding Oracle Attestations
```bash
bitcoin-s-cli decodeattestments fdd8687f194254432d5553442d4f5645522d35304b2d434f494e424153451d5dcdba2e64cb116cc0c375a0856298f0058b778f46bfe625ac6576204889e40001424c11a44c2e522f90bbe4abab6ec1bc8ab44c9b29316ce6e1d0d7d08385a474de6b75f1da183a2a4f9ad144b48bf1026cee9687221df58f04128db79ca17e2a024e4f
{
  "eventId": "BTC-USD-OVER-50K-COINBASE",
  "signatures": [
    "424c11a44c2e522f90bbe4abab6ec1bc8ab44c9b29316ce6e1d0d7d08385a474de6b75f1da183a2a4f9ad144b48bf1026cee9687221df58f04128db79ca17e2a"
  ],
  "values": [
    "NO"
  ]
}
```

## Step 3: Set up The DLC

### Using the GUI 

If you're a visual learner there is a [video demo](https://www.youtube.com/watch?v=zy1sL2ndcDg) that explains this process in detail. 
But do note that this demonstrates the old non-adaptor version of DLCs so that the Offer, Accept, Sign protocol is the same, but the contents will be different.

If using a numeric contract and/or multiple oracles, messages can get very large and sometimes even too large to for the application.
To solve this there is an `Export to file` button located under the text box for the messages your wallet will construct.
This can be used to export a DLC message to a file and then the file can be sent to your counterparty.
If you receive a file from a counter-party, there is an `Import file` button on every dialog you input a DLC message.
This can be used to import the file of the DLC message from your counter-party.

#### Creating The Offer

Once the terms are agreed to, either party can use the `Offer` button and enter each of the fields from the table above.

#### Accepting The Offer

Upon receiving a DLC Offer from your counter-party, you can use the `Accept` button and paste in the DLC Offer.

#### Signing The DLC

Upon receiving a DLC Accept message from your counter-party, you can use the `Sign` button and paste in the DLC Accept.

#### Adding DLC Signatures To Your Database

Upon receiving a DLC Sign message from your counter-party, add their signatures to your database using the `Add Sigs` button and paste in the message.
After doing so you can get the fully signed funding transaction using the `Get Funding Tx` button. This will return the fully signed serialized transaction.

### Using the CLI

If using a numeric contract and/or multiple oracles, messages can get very large and sometimes even too large to for the application.
To solve this there are RPC calls where you can give a file instead of the entire DLC message.
To output a file you simply just need to pipe the output of a command into a file.

For example:
```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli acceptdlcoffer [offer] > myDLCAccept.txt
```

#### Creating The Offer

Once these terms are agreed to, either party can call on `createdlcoffer` with flags for each of the fields in the table above. For example:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli createdlcoffer [contractInfo] [collateral] [feerate] [locktime] [refundlocktime]
```

#### Accepting The Offer

Upon receiving a DLC Offer from your counter-party, the following command will create the serialized accept message:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli acceptdlcoffer [offer]
```

or from file:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli acceptdlcofferfromfile [filepath]
```

#### Signing The DLC

Upon receiving a DLC Accept message from your counter-party, the following command will generate all of your signatures for this DLC:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli signdlc [accept]
```

or from file:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli signdlcfromfile [filepath]
```


#### Adding DLC Signatures To Your Database

Upon receiving a DLC Sign message from your counter-party, add their signatures to your database by:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli adddlcsigs [sign]
```

or from file:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli adddlcsigsfromfile [filepath]
```

#### Getting Funding Transaction

You are now fully setup and can generate the fully signed funding transaction for broadcast using

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli getdlcfundingtx [contractId]
```

where the `contractId` is in all but the messages other than the DLC Offer message, and is also returned by the `adddlcsigs` command.

Alternatively, you can use the `getdlcs` command to list all of your current DLCs saved in your wallet.

## Step 4: Executing the DLC

### Using the GUI

#### Execute

You can execute the DLC unilaterally with the `Execute` button which will require the oracle signature.
This will return a fully signed Contract Execution Transaction for the event signed by the oracle.

#### Refund

If the `refundlocktime` for the DLC has been reached, you can get the fully-signed refund transaction with the `Refund` button and entering the `contractId`.

### Using the CLI

#### Execute

Upon receiving an oracle signature, you can execute the DLC unilaterally with

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli executedlc [contractId] [signatureTLVs]
```

which will return fully signed Contract Execution Transaction for the event signed by the oracle.

#### Refund

If the `refundlocktime` for the DLC has been reached, you can get the fully-signed refund transaction with

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli executedlcrefund [contractId]
```
