---
id: oracle-server
title: Oracle Server
---

The Oracle Server is a DLC Oracle with functionality for creating events and attesting to them.
You can interact with the oracle server with `bitcoin-s-cli` or `curl`

The following a guide is for interacting with the oracle
If you are looking for the documentation on how to build the oracle server,
checkout [this page](build-oracle-server.md).

## Server Endpoints

- `getpublickey` - Get oracle's public key
- `getstakingaddress` - Get oracle's staking address
- `listevents` - Lists all event names
- `createenumevent` `label` `maturationtime` `outcomes` - Registers an oracle enum event
  - `label` - Label for this event
  - `maturationtime` - The earliest expected time an outcome will be signed, given in ISO 8601 format
  - `outcomes` - Possible outcomes for this event
- `createnumericevent` `name` `maturationtime` `minvalue` `maxvalue` `unit` `precision` - Registers an oracle event that uses digit decomposition when signing the number
  - `name`- Name for this event
  - `maturationtime` - The earliest expected time an outcome will be signed, given in ISO 8601 format
  - `minvalue` - Minimum value of this event
  - `maxvalue` - Maximum value of this event
  - `unit` - The unit denomination of the outcome value
  - `precision` - The precision of the outcome representing the base exponent by which to multiply the number represented by the composition of the digits to obtain the actual outcome value.
- `createdigitdecompevent` `name` `maturationtime` `base` `numdigits` `unit` `precision` `[signed]` - Registers an oracle event that uses digit decomposition when signing the number
  - `name`- Name for this event
  - `maturationtime` - The earliest expected time an outcome will be signed, given in epoch second
  - `base` - The base in which the outcome value is decomposed
  - `numdigits` - The max number of digits the outcome can have
  - `unit` - The unit denomination of the outcome value
  - `precision` - The precision of the outcome representing the base exponent by which to multiply the number represented by the composition of the digits to obtain the actual outcome value.
  - `--signed`- Whether the outcomes can be negative
- `getevent` `event` - Get an event's details
  - `eventName` - The event's name
- `signevent` `event` `outcome` - Signs an event
    - `eventName` - The event's name
    - `outcome`- Outcome to sign for this event
- `signdigits` `event` `outcome` - Signs an event
  - `eventName` - The event's name
  - `outcome` - Number to sign for this event
- `getsignatures` `event` - Get the signatures from a signed event
  - `eventName` - The event's name
  
### Create Event Example

Bitcoin-S CLI:
```bash
$ bitcoin-s-cli createenumevent test "2030-01-03T00:30:00.000Z" "outcome1,outcome2,outcome3"
fdd824b0ba0f08e9becbf77019e246ca8a80c027585634dc1aed4b7f67442ada394b40dcb242d8a8c84893a752b93f30ff07525b0604382255ec7392fcc6f230140feb905f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e52115fdd8224c000180e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f657131d1fdd8061d0003086f7574636f6d6531086f7574636f6d6532086f7574636f6d65330474657374

$ bitcoin-s-cli getevent test
{
  "nonces": [
    "80e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f"
  ],
  "eventName": "test",
  "signingVersion": "DLCOracleV0SigningVersion",
  "maturationTime": "2030-01-03T00:30:00.000Z",
  "announcementSignature": "ba0f08e9becbf77019e246ca8a80c027585634dc1aed4b7f67442ada394b40dcb242d8a8c84893a752b93f30ff07525b0604382255ec7392fcc6f230140feb90",
  "eventDescriptorTLV": "fdd8061d0003086f7574636f6d6531086f7574636f6d6532086f7574636f6d6533",
  "eventTLV": "fdd8224c000180e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f657131d1fdd8061d0003086f7574636f6d6531086f7574636f6d6532086f7574636f6d65330474657374",
  "announcementTLV": "fdd824b0ba0f08e9becbf77019e246ca8a80c027585634dc1aed4b7f67442ada394b40dcb242d8a8c84893a752b93f30ff07525b0604382255ec7392fcc6f230140feb905f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e52115fdd8224c000180e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f657131d1fdd8061d0003086f7574636f6d6531086f7574636f6d6532086f7574636f6d65330474657374",
  "attestations": null,
  "signatures": null,
  "outcomes": [
    "outcome1",
    "outcome2",
    "outcome3"
  ],
  "signedOutcome": null
}


$ bitcoin-s-cli signevent test "outcome1"
fdd8687004746573745f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e52115000180e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f33fd84ba8eea0a75f1568149f42e8466e1bc3422ea449532d4eeffad8586d14e086f7574636f6d6531

$ bitcoin-s-cli getsignatures test
fdd8687004746573745f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e52115000180e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f33fd84ba8eea0a75f1568149f42e8466e1bc3422ea449532d4eeffad8586d14e086f7574636f6d6531
```

CURL:
```bash
$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "createenumevent", "params": ["testEvent", ""2030-01-03T00:30:00.000Z"", ["outcome1", "outcome2", "outcome3"]]}' -H "Content-Type: application/json" http://127.0.0.1:9998/
{"result":"fdd824b0ba0f08e9becbf77019e246ca8a80c027585634dc1aed4b7f67442ada394b40dcb242d8a8c84893a752b93f30ff07525b0604382255ec7392fcc6f230140feb905f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e52115fdd8224c000180e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f657131d1fdd8061d0003086f7574636f6d6531086f7574636f6d6532086f7574636f6d65330474657374","error":null}

$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getevent", "params": ["testEvent"]}' -H "Content-Type: application/json" http://127.0.0.1:9998/
{"result":{"nonces":["80e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f"],"eventName":"test","signingVersion":"DLCOracleV0SigningVersion","maturationTime":"2030-01-03T00:30:00.000Z","announcementSignature":"ba0f08e9becbf77019e246ca8a80c027585634dc1aed4b7f67442ada394b40dcb242d8a8c84893a752b93f30ff07525b0604382255ec7392fcc6f230140feb90","eventDescriptorTLV":"fdd8061d0003086f7574636f6d6531086f7574636f6d6532086f7574636f6d6533","eventTLV":"fdd8224c000180e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f657131d1fdd8061d0003086f7574636f6d6531086f7574636f6d6532086f7574636f6d65330474657374","announcementTLV":"fdd824b0ba0f08e9becbf77019e246ca8a80c027585634dc1aed4b7f67442ada394b40dcb242d8a8c84893a752b93f30ff07525b0604382255ec7392fcc6f230140feb905f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e52115fdd8224c000180e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f657131d1fdd8061d0003086f7574636f6d6531086f7574636f6d6532086f7574636f6d65330474657374","attestations":["33fd84ba8eea0a75f1568149f42e8466e1bc3422ea449532d4eeffad8586d14e"],"signatures":["80e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f33fd84ba8eea0a75f1568149f42e8466e1bc3422ea449532d4eeffad8586d14e"],"outcomes":["outcome1","outcome2","outcome3",],"signedOutcome": null},"error":null}

$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "signevent", "params": ["testEvent", "outcome1"]}' -H "Content-Type: application/json" http://127.0.0.1:9998/
{"result":"fdd8687004746573745f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e52115000180e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f33fd84ba8eea0a75f1568149f42e8466e1bc3422ea449532d4eeffad8586d14e086f7574636f6d6531","error":null}

$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getsignatures", "params": ["testEvent"]}' -H "Content-Type: application/json" http://127.0.0.1:9998/
{"result":["fdd8687004746573745f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e52115000180e550759cb6275f6db3fad2b616ed51bdcccc204d0d978cd921cafae9fc1d6f33fd84ba8eea0a75f1568149f42e8466e1bc3422ea449532d4eeffad8586d14e086f7574636f6d6531"],"error":null}
```

### Numeric Example

Bitcoin-S CLI:

```bash
$ bitcoin-s-cli createnumericevent exampleNumeric "2030-01-03T00:30:00.000Z" -1000 1000 "units" 0
fdd824fd010bfc52dab25169eef25815c795128f38ef3b89bc7f53d1d788b4a1c544e5bebfbf6799975b62a1888e2d77445b6d002672f52f8626b1ea6cc6cd974a8039d28a9f5f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e52115fdd822a70004012d73a453bb630fe355830a81727cf2fb10c41ccfee040c529a4dec21ca5071f5aff60ac9ef8425ae438e84a6f067952d60b947e9e44bfc6e8fd89b781492057b1db5da37f1c10bfcaf7a4fb0e9f6dbb8d25dfed7a25241bbec3c0a60a40d2949305ff92f679598a11e7a857beef901903fc83624413831a06513da577cdd66657131d1fdd80a0f000a0105756e6974730000000000030d6578616d706c654465636f6d70

$ bitcoins-cli getevent exampleNumeric
{
  "nonces": [
    "012d73a453bb630fe355830a81727cf2fb10c41ccfee040c529a4dec21ca5071",
    "49305ff92f679598a11e7a857beef901903fc83624413831a06513da577cdd66",
    "7b1db5da37f1c10bfcaf7a4fb0e9f6dbb8d25dfed7a25241bbec3c0a60a40d29",
    "f5aff60ac9ef8425ae438e84a6f067952d60b947e9e44bfc6e8fd89b78149205"
  ],
  "eventName": "exampleNumeric",
  "signingVersion": "DLCOracleV0SigningVersion",
  "maturationTime": "2030-01-03T00:30:00.000Z",
  "announcementSignature": "fc52dab25169eef25815c795128f38ef3b89bc7f53d1d788b4a1c544e5bebfbf6799975b62a1888e2d77445b6d002672f52f8626b1ea6cc6cd974a8039d28a9f",
  "eventDescriptorTLV": "fdd80a0f000a0105756e697473000000000003",
  "eventTLV": "fdd822a70004012d73a453bb630fe355830a81727cf2fb10c41ccfee040c529a4dec21ca507149305ff92f679598a11e7a857beef901903fc83624413831a06513da577cdd667b1db5da37f1c10bfcaf7a4fb0e9f6dbb8d25dfed7a25241bbec3c0a60a40d29f5aff60ac9ef8425ae438e84a6f067952d60b947e9e44bfc6e8fd89b78149205657131d1fdd80a0f000a0105756e6974730000000000030d6578616d706c654465636f6d70",
  "announcementTLV": "fdd824fd010bfc52dab25169eef25815c795128f38ef3b89bc7f53d1d788b4a1c544e5bebfbf6799975b62a1888e2d77445b6d002672f52f8626b1ea6cc6cd974a8039d28a9f5f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e52115fdd822a70004012d73a453bb630fe355830a81727cf2fb10c41ccfee040c529a4dec21ca507149305ff92f679598a11e7a857beef901903fc83624413831a06513da577cdd667b1db5da37f1c10bfcaf7a4fb0e9f6dbb8d25dfed7a25241bbec3c0a60a40d29f5aff60ac9ef8425ae438e84a6f067952d60b947e9e44bfc6e8fd89b78149205657131d1fdd80a0f000a0105756e6974730000000000030d6578616d706c654465636f6d70",
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
  ],
  "signedOutcome": null
}


$ bitcoin-s-cli signdigits exampleNumeric 123
fdd868fd01380d6578616d706c654465636f6d705f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e521150004012d73a453bb630fe355830a81727cf2fb10c41ccfee040c529a4dec21ca5071a853a189b9acffa2488542c4998261866ce392dbf38031509ceff34077431e65f5aff60ac9ef8425ae438e84a6f067952d60b947e9e44bfc6e8fd89b78149205773713008d316640b74d04f180b6c3c09b8de11b29cd7474681a7ad869857cd57b1db5da37f1c10bfcaf7a4fb0e9f6dbb8d25dfed7a25241bbec3c0a60a40d294f2222871b23a823acbfa552478ae3d526377a8918b346d6e206156c3e5a2c8549305ff92f679598a11e7a857beef901903fc83624413831a06513da577cdd66c03ed28ef6f7b0f48f974b61811a571652ea2eafda5fd5b244674420deb294e8012b013101320133
```

CURL:

```bash
$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "createnumericevent", "params": ["numericExample", "2030-01-03T00:30:00.000Z", -1000, 1000, "units", 0]}' -H "Content-Type: application/json" http://127.0.0.1:9998/
{"result":"fdd824fd0110647c85d333aa6fc0d7808201da9d1010b815710dc25c3d73e9cc7a7f372a7342c99144ba74d70be72920f4515daa6565bce12aedfc5a828ee37b5453454c1b575f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e52115fdd822ac0004d72282a2e9532924dc8cd79685a501202332ad0d118166328cb76138414fccf37051e50fd1ab30df4717da8905e400a32c5f4d793a4ac5433ed416165dd286c47446ab1d71a550a0d604c3e86c40a3c9b12de8f08a86639068707822cd4756217139d7cabd19d6b0b9e827cdf84a4fc18c88d1882e4e096d8dfeff58759504d2657131d1fdd80a0f000a0105756e697473000000000003126578616d706c6544696769744465636f6d70","error":null}

$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getevent", "params": ["numericExample"]}' -H "Content-Type: application/json" http://127.0.0.1:9998/
{"result":{"nonces":["7051e50fd1ab30df4717da8905e400a32c5f4d793a4ac5433ed416165dd286c4","7139d7cabd19d6b0b9e827cdf84a4fc18c88d1882e4e096d8dfeff58759504d2","7446ab1d71a550a0d604c3e86c40a3c9b12de8f08a86639068707822cd475621","d72282a2e9532924dc8cd79685a501202332ad0d118166328cb76138414fccf3"],"eventName":"numericExample","signingVersion":"DLCOracleV0SigningVersion","maturationTime":"2030-01-03T00:30:00.000Z","announcementSignature":"647c85d333aa6fc0d7808201da9d1010b815710dc25c3d73e9cc7a7f372a7342c99144ba74d70be72920f4515daa6565bce12aedfc5a828ee37b5453454c1b57","eventDescriptorTLV":"fdd80a0f000a0105756e697473000000000003","eventTLV":"fdd822ac00047051e50fd1ab30df4717da8905e400a32c5f4d793a4ac5433ed416165dd286c47139d7cabd19d6b0b9e827cdf84a4fc18c88d1882e4e096d8dfeff58759504d27446ab1d71a550a0d604c3e86c40a3c9b12de8f08a86639068707822cd475621d72282a2e9532924dc8cd79685a501202332ad0d118166328cb76138414fccf3657131d1fdd80a0f000a0105756e697473000000000003126578616d706c6544696769744465636f6d70","announcementTLV":"fdd824fd0110647c85d333aa6fc0d7808201da9d1010b815710dc25c3d73e9cc7a7f372a7342c99144ba74d70be72920f4515daa6565bce12aedfc5a828ee37b5453454c1b575f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e52115fdd822ac00047051e50fd1ab30df4717da8905e400a32c5f4d793a4ac5433ed416165dd286c47139d7cabd19d6b0b9e827cdf84a4fc18c88d1882e4e096d8dfeff58759504d27446ab1d71a550a0d604c3e86c40a3c9b12de8f08a86639068707822cd475621d72282a2e9532924dc8cd79685a501202332ad0d118166328cb76138414fccf3657131d1fdd80a0f000a0105756e697473000000000003126578616d706c6544696769744465636f6d70","attestations":null,"signatures":null,"outcomes":[["0","1","2","3","4","5","6","7","8","9"],["0","1","2","3","4","5","6","7","8","9"],["0","1","2","3","4","5","6","7","8","9"],["+","-"]],"signedOutcome": null},"error":null}

$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "signdigits", "params": ["numericExample", 123]}' -H "Content-Type: application/json" http://127.0.0.1:9998/
{"result":"fdd868fd013d126578616d706c6544696769744465636f6d705f6f49e116de8cb57856bacdd9997d8dfb73877f64a4ec8d45fc0e73a0e521150004d72282a2e9532924dc8cd79685a501202332ad0d118166328cb76138414fccf3d0646c9efd9523274014841ba24bf63219d5650d1682209d7e48af009d58e6d87051e50fd1ab30df4717da8905e400a32c5f4d793a4ac5433ed416165dd286c4c025dfd1e39de77e0418fa7d39abf2e9daf55d7fe34f8e312368cb4d45b4d4b97446ab1d71a550a0d604c3e86c40a3c9b12de8f08a86639068707822cd475621700347c52af088eda9a0245385094518134e73bb997102e11f6de0aeb36af7237139d7cabd19d6b0b9e827cdf84a4fc18c88d1882e4e096d8dfeff58759504d2f9e7a9e183b0836ad58dd646d9ab123132397109e4f51c5842958932a81bacd1012b013101320133","error":null}
```
