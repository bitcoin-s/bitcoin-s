---
id: version-1.8.0-oracle-explorer-client
title: Oracle Explorer Client
original_id: oracle-explorer-client
---

[Suredbits offers a tool called an Oracle Explorer](https://oracle.suredbits.com) for oracles to post their
announcements and attestments at a later time.

Bitcoin-s provides an open source oracle explorer client for
interacting with the oracle explorer.

### Environments

There are 2 live environments that can be used with the explorer client

1. ExplorerEnv.Production
2. ExplorerEnv.Test

As the names indicate, one references the [production oracle explorer](https://oracle.suredbits.com)
while the other references the [test environment](https://test.oracle.suredbits.com)

```scala
implicit val system = ActorSystem("explorer-client-actor-system")

//use test environment for this little example
val env = ExplorerEnv.Test
val explorerClient = SbExplorerClient(env, proxyParams = None)

//list all announcemnts on the explorer
val announcementsF: Future[Vector[SbAnnouncementEvent]] = explorerClient.listAnnouncements()


//example announcement taken from
//https://oracle.suredbits.com/event/e0a5624edbc854120982165b0eef53f0777a49febd79a0c21bf75e5582021e33
val announcementHex = "fdd824c8bf634b2d76f6d8c6499aa977a7b0ae2b84bc206d800f8448e46d63d6ca31778ddb023e39df098c7e109b3d6ee7273d18be62e10f8481dae6531dbe3e0647f6e95d1bcfab252c6dd9edd7aea4c5eeeef138f7ff7346061ea40143a9f5ae80baa9fdd82264000190ef605e3450e16b47745e7a33e26ac9437f6ec2ed660d829a064dceee3699c8605fc700fdd806270005076e67616e6e6f75066d696f63696304647261770a6e6f2d636f6e74657374056f74686572124d696f6369632076204e67616e6e6f752032"
val announcement = OracleAnnouncementV0TLV.fromHex(announcementHex)

//you can query by announcement
val announcementF: Future[SbAnnouncementEvent] = explorerClient.getAnnouncement(announcement)

//or query by hash 
val sameAnnouncementF: Future[SbAnnouncementEvent] = explorerClient.getAnnouncement(announcement.sha256)

//you can post an announcement to the oracle explorer
val oracleName = "Chris_Stewart_5"
val description = "2021-03-24-sunny-in-chicago"
val uriOpt = Some("https://twitter.com/Chris_Stewart_5")

val sbAnnouncement = CreateAnnouncementExplorer(announcement, oracleName, description, uriOpt)

val createdF = explorerClient.createAnnouncement(sbAnnouncement)

//and then you can follow up and post the attestations
val attestationsHex =
  "fdd868821b323032312d30332d32342d73756e6e792d696e2d6368696361676f1d5dcdba2e64cb116cc0c375a0856298f0058b778f46bfe625ac6576204889e40001efdf735567ae0a00a515e313d20029de5d7525da7b8367bc843d28b672d4db4db5de4dbff689f3b742be634a9c92c615dbcf2eadbdd470f514b1ac250a30db6d03594553"

val attestations = OracleAttestmentV0TLV.fromHex(attestationsHex)

val announcementHash = announcement.sha256
val sbAttestations = CreateAttestations(announcementHash, attestations)
val createdAttestationsF = explorerClient.createAttestations(sbAttestations)
```
