---
id: oracle-election-example
title: Election Example
---

## Requirements for example

You need to have a fully built oracle server. You can follow [this guide](build-oracle-server.md) to do this.

You will also need a the `bitcoin-s-cli` command line tool to interact with the server.
You can find how to build this [here](../applications/cli.md)

After building the oracle server, you will need to start it with

```
./app/oracle-server/target/universal/stage/bin/bitcoin-s-oracle-server
```

## US 2020 Election

In 2020, the United States held a presidential election.
People want to do a DLC based on the outcome, so you decide to be
their oracle and attest to the winner of the election.

1. Setting up an oracle that can attest to the outcome of the election
2. Complete the event by attesting to the winner of the election

### Setting up the election bet

The election bet can be represented by an enumerated event. What we mean by this is the outcomes
for this event can be simply enumerated in a list. There were three possible outcomes for the election

1. "Republican_win"
2. "Democrat_win"
3. "other"

The next step is to create an oracle announcement that can be shared with others. This announcement
contains cryptographic information that is used by people that want to enter into DLCs to setup
their bitcoin transactions.

To do this, we need to use the `createenumevent` rpc. This RPC takes 3 parameters

1. The label for the event
   we will use `2020-us-election`
2. the maturation time for the event in ISO 8061 format
   For our example we will use `"2021-01-20T00:00:00Z"`
3. The outcomes, which we listed above "Republican_win,Democrat_win,other"

With all of this information, we can create the event with `bitcoin-s-cli`!

```
./bitcoin-s-cli createenumevent 2020-us-election "2021-01-20T00:00:00Z" "Republican_win,Democrat_win,other"
fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e
```

Yay! The hex string returned is an oracle announcement.
You can submit this on a tool like the [Suredbits oracle explorer](https://oracle.suredbits.com)
so others can find your oracle.

If you are building infrastructure to automatically sign events, it is important to store two things

1. The oracle announcement above (`fdd824c...`)
2. The timestamp that the event matures at (`"2021-01-20T00:00:00Z"`)

Now you can schedule jobs to sign the event when the maturation time passes.

### Signing the outcome for the election bet

In the real world, you would want to wait for the maturation time to pass for your event.
For the purposes of the demo, we can skip this wait. The winner of the US election was Joe Biden.

Let's sign the event.

```
 ./bitcoin-s-cli signevent 2020-us-election Democrat_win
fdd8688010323032302d75732d656c656374696f6ed3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000a0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1447a59ba58797e55b967aa79c89ffec67023578009c4dc1e3dee2fd75277993590c44656d6f637261745f77696e
```

Yay! Now bitcoin-s gives us an attestation that is represented by the hex string `fdd868...`

If you submitted your event to the [suredbits oracle explorer](https://oracle.suredbits.com) above
you will also want to submit the attestation for your event so others can find it and settle their DLCs.

If you use the `getevent` rpc along the oracle announcement, you can see the event is now completed!

```
 ./bitcoin-s-cli getevent 2020-us-election
{
  "nonces": [
    "ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee14"
  ],
  "eventName": "2020-us-election",
  "signingVersion": "DLCOracleV0SigningVersion",
  "maturationTime": "2021-01-20T00:00:00Z",
  "announcementSignature": "988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5",
  "eventDescriptorTLV": "fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f74686572",
  "eventTLV": "fdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e",
  "announcementTLV": "fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e",
  "attestations": [
    "47a59ba58797e55b967aa79c89ffec67023578009c4dc1e3dee2fd7527799359"
  ],
  "signatures": [
    "ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1447a59ba58797e55b967aa79c89ffec67023578009c4dc1e3dee2fd7527799359"
  ],
  "outcomes": [
    "Republican_win",
    "Democrat_win",
    "other"
  ],
  "signedOutcome": "Democrat_win"
}
```
