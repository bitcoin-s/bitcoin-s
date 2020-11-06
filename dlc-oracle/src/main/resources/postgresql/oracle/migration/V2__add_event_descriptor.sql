-- Since we will be dropping the events table we need to cache the event outcome table
CREATE TEMP TABLE event_outcomes_temp
(
    nonce          TEXT NOT NULL,
    message        TEXT NOT NULL,
    hashed_message TEXT NOT NULL
);
INSERT INTO event_outcomes_temp
SELECT nonce, message, hashed_message
FROM event_outcomes;
DROP TABLE event_outcomes;

-- Move announcement sig to event table, add event_descriptor_tlv column as well
CREATE TEMPORARY TABLE events_backup
(
    nonce                  TEXT      NOT NULL,
    pubkey                 TEXT      NOT NULL,
    nonce_index            INTEGER   NOT NULL DEFAULT 0,
    event_name             TEXT      NOT NULL,
    num_outcomes           INTEGER   NOT NULL,
    signing_version        TEXT      NOT NULL,
    maturation_time        TIMESTAMP NOT NULL,
    attestation            TEXT,
    announcement_signature TEXT      NOT NULL,
    event_descriptor_tlv   TEXT      NOT NULL DEFAULT 'fdd806090001000564756d6d79'
);
INSERT INTO events_backup (nonce, pubkey, event_name, num_outcomes, signing_version, maturation_time,
                           announcement_signature, attestation)
SELECT e.nonce,
       e.pubkey,
       e.event_name,
       e.num_outcomes,
       e.signing_version,
       e.maturation_time,
       r.announcement_signature,
       e.attestation
FROM events e,
     r_values r
WHERE r.nonce = e.nonce;
DROP TABLE events;
CREATE TABLE events
(
    nonce                  TEXT      NOT NULL PRIMARY KEY,
    pubkey                 TEXT      NOT NULL,
    nonce_index            INTEGER   NOT NULL,
    event_name             TEXT      NOT NULL,
    num_outcomes           INTEGER   NOT NULL,
    signing_version        TEXT      NOT NULL,
    maturation_time        TIMESTAMP NOT NULL,
    attestation            TEXT,
    announcement_signature TEXT      NOT NULL,
    event_descriptor_tlv   TEXT      NOT NULL
);
INSERT INTO events (nonce, pubkey, nonce_index, event_name, num_outcomes, signing_version, maturation_time,
                    announcement_signature, attestation, event_descriptor_tlv)
SELECT nonce,
       pubkey,
       nonce_index,
       event_name,
       num_outcomes,
       signing_version,
       maturation_time,
       announcement_signature,
       attestation,
       event_descriptor_tlv
FROM events_backup;
DROP TABLE events_backup;

-- Drop announcement sig column from R value table
CREATE TEMPORARY TABLE r_values_backup
(
    nonce         TEXT    NOT NULL,
    event_name    TEXT    NOT NULL,
    hd_purpose    INTEGER NOT NULL,
    coin          INTEGER NOT NULL,
    account_index INTEGER NOT NULL,
    chain_type    INTEGER NOT NULL,
    key_index     INTEGER NOT NULL UNIQUE
);
INSERT INTO r_values_backup
SELECT nonce, event_name, hd_purpose, coin, account_index, chain_type, key_index
FROM r_values;
DROP TABLE r_values;
CREATE TABLE r_values
(
    nonce         TEXT    NOT NULL,
    event_name    TEXT    NOT NULL,
    hd_purpose    INTEGER NOT NULL,
    coin          INTEGER NOT NULL,
    account_index INTEGER NOT NULL,
    chain_type    INTEGER NOT NULL,
    key_index     INTEGER NOT NULL UNIQUE,
    PRIMARY KEY (nonce)
);

INSERT INTO r_values
SELECT nonce, event_name, hd_purpose, coin, account_index, chain_type, key_index
FROM r_values_backup;
DROP TABLE r_values_backup;

-- recreate outcome table
CREATE TABLE event_outcomes
(
    nonce          TEXT NOT NULL,
    message        TEXT NOT NULL,
    hashed_message TEXT NOT NULL,
    CONSTRAINT fk_nonce FOREIGN KEY (nonce) REFERENCES events (nonce) on update NO ACTION on delete NO ACTION
);
INSERT INTO event_outcomes
SELECT nonce, message, hashed_message
FROM event_outcomes_temp;
DROP TABLE event_outcomes_temp;