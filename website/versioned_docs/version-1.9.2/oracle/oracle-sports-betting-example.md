---
id: version-1.9.2-oracle-sports-betting-example
title: Sports Betting Example
original_id: oracle-sports-betting-example
---

## Requirements for example

You need to have a fully built oracle server. You can follow [this guide](build-oracle-server.md) to do this.

You will also need a the `bitcoin-s-cli` command line tool to interact with the server.
You can find how to build this [here](../applications/cli.md)

After building the oracle server, you will need to start it with

```
./app/oracle-server/target/universal/stage/bin/bitcoin-s-oracle-server
```

## Rocky V.S. Drago 2022 Boxing Championship

In 2022, Rocky and Drago compete in the final round of the boxing championship.
People want to do a DLC based on the outcome, so you decide to be
their oracle and attest to the winner of the match.

1. Setting up an oracle that can attest to the outcome of the match
2. Complete the announcement by attesting to the winner of the match

### Setting up the boxing bet

The boxing match bet can be represented by an enumerated announcement. What we mean by this is the outcomes for this announcement can be simply enumerated in a list. There were three possible outcomes for the match

1. "Drago_win"
2. "Rocky_win"
3. "other"

The next step is to create an oracle announcement that can be shared with others. This announcement
contains cryptographic information that is used by people that want to enter into DLCs to setup
their bitcoin transactions.

To do this, we need to use the `createenumannouncement` rpc. This RPC takes 3 parameters

1. The label for the announcement
   we will use `2022-championship-boxing`
2. the maturation time for the announcement in ISO 8061 format
   For our example we will use `"2022-06-16T00:00:00Z"`
3. The outcomes, which we listed above "Drago_win,Rocky_win,other"

with all of this information, we can create the announcement with `bitcoin-s-cli`!

./bitcoin-s-cli createenumannouncement 2022-championship-boxing "2022-06-16T00:00:00Z" "Rocky_win,Drago_win,other"
fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e67

Yay! The hex string returned is an oracle announcement.
You can submit this on a tool like the [Suredbits oracle explorer](https://oracle.suredbits.com)
so others can find your oracle.

If you are building infrastructure to automatically sign announcements, it is important to store two things

1. The oracle announcement above (`fdd824c...`)
2. The timestamp that the announcement matures at (`"2022-06-16T00:00:00Z"`) 

Now you can schedule jobs to sign the announcement when the maturation time passes.

### Signing the outcome for the boxing bet

In the real world, you would want to wait for the maturation time to pass for your announcement.
For the purposes of the demo, we can skip this wait. The winner of the 2022 boxing championship is Rocky.

Let's sign the announcement.

```
 ./bitcoin-s-cli signenum 2022-championship-boxing Rocky_win
fdd8688518323032322d6368616d70696f6e736869702d626f78696e67b923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cb00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be7c83c64f33bacceb800f463a3a98ca56a40bad7c5e7a417e3f4e012f0f332d4c09526f636b795f77696e
```

Yay! Now bitcoin-s gives us an attestation that is represented by the hex string `fdd868...`

If you submitted your announcement to the [suredbits oracle explorer](https://oracle.suredbits.com) above
you will also want to submit the attestation for your announcement so others can find it and settle their DLCs.

If you use the `getannouncement` rpc along the oracle announcement, you can see the announcement is now completed!

```
./bitcoin-s-cli getannouncement 2022-championship-boxing
{
  "nonces": [
    "0a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be"
  ],
  "eventName": "2022-championship-boxing",
  "signingVersion": "DLCOracleV0SigningVersion",
  "maturationTime": "2022-06-16T00:00:00Z",
  "maturationTimeEpoch": 1655337600,
  "announcementSignature": "58412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5e",
  "eventDescriptorTLV": "fdd8061c000309526f636b795f77696e09447261676f5f77696e056f74686572",
  "eventTLV": "fdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e67",
  "announcementTLV": "fdd824c358412e9c9dc12942a9e1ed030c7bf4ff10f80b0228f9674245ae15d8a956bae6fa0186994d7e12a3b0793b67ebad83d735e9232d9e15eda5c2948d4b8a042a5eb923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cbfdd8225f00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be62aa7280fdd8061c000309526f636b795f77696e09447261676f5f77696e056f7468657218323032322d6368616d70696f6e736869702d626f78696e67",
  "attestations": "fdd8688518323032322d6368616d70696f6e736869702d626f78696e67b923f087791a31409523431b28dc20af6871f515715ab0e6219c971ee4cb75cb00010a9e74e8c52ae90846cedbb2af3d2e0edc2234a8b4ef240c4fcfbe4f31c693be7c83c64f33bacceb800f463a3a98ca56a40bad7c5e7a417e3f4e012f0f332d4c09526f636b795f77696e",
  "outcomes": [
    "Rocky_win",
    "Drago_win",
    "other"
  ],
  "signedOutcome": "Rocky_win",
```
