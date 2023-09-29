create table "oracle_metadata" (
    "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
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
    -- this should be set to unique as done in the oracle project
    -- for now i'm not sure how to recover the origin attestation
    -- for this _specific_ nonce. Right now we just store the aggregate attestation
    "attestation" VARCHAR(32),
    "nonce_proof" VARCHAR(64) NOT NULL UNIQUE,
    "outcome" VARCHAR(254),
    constraint "fk_metadata_id" foreign key("id") references "oracle_metadata"("id") on update NO ACTION on delete NO ACTION
);

CREATE TABLE "event_outcomes"
(
    `nonce`          VARCHAR(64) NOT NULL,
    `message`        VARCHAR(254) NOT NULL,
    `hashed_message` VARCHAR(32) NOT NULL
);