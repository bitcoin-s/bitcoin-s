
create table "oracle_metadata" (
    "id" INTEGER PRIMARY KEY NOT NULL,
    "public_key" VARCHAR(32) NOT NULL,
    "oracle_name" VARCHAR(254) NOT NULL,
    "oracle_description" VARCHAR(254) NOT NULL,
    "creation_time" INTEGER NOT NULL,
    "oracle_metadata_signature" VARCHAR(64) NOT NULL,
    "attestation_public_key" VARCHAR(32) NOT NULL,
    "attestation_public_key_signature" VARCHAR(64) NOT NULL
);


create table "oracle_schnorr_nonces" (
    "id" INTEGER NOT NULL,
    "nonce" VARCHAR(32) NOT NULL UNIQUE,
    "attestation" VARCHAR(32) UNIQUE,
    "nonce_proof" VARCHAR(64) NOT NULL UNIQUE,
    "outcome" VARCHAR(254),
    constraint "fk_metadata_id" foreign key("id") references "oracle_metadata"("id") on update NO ACTION on delete NO ACTION
);

--from dlc wallet, we need table that can represent an announcement with a single row

CREATE TABLE "oracle_announcement_data"
(
    "id"                     INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "announcement_signature" VARCHAR(254)                      NOT NULL UNIQUE,
    "pub_key"                VARCHAR(254)                      NOT NULL,
    "signing_pub_key"        VARCHAR(254)                      NOT NULL,
    "event_maturity"         INTEGER                           NOT NULL,
    "event_id"               VARCHAR(254)                      NOT NULL,
    "event_descriptor"       VARCHAR(254)                      NOT NULL
);
CREATE
    INDEX "oracle_announcements_pub_key_index" on "oracle_announcement_data" ("pub_key");