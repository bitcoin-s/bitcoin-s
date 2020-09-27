CREATE TABLE "r_values"
(
    "nonce"         TEXT PRIMARY KEY,
    "hd_purpose"    INTEGER NOT NULL,
    "coin"          INTEGER NOT NULL,
    "account_index" INTEGER NOT NULL,
    "chain_type"    INTEGER NOT NULL,
    "key_index"     INTEGER NOT NULL
);

CREATE TABLE "events"
(
    "nonce"           TEXT PRIMARY KEY,
    "label"           TEXT UNIQUE NOT NULL,
    "num_outcomes"    INTEGER     NOT NULL,
    "signing_version" INTEGER     NOT NULL,
    "attestation"     TEXT,
    constraint "fk_nonce" foreign key ("nonce") references "r_values" ("nonce")
);

CREATE TABLE "event_outcomes"
(
    "nonce"          TEXT PRIMARY KEY,
    "message"        TEXT NOT NULL,
    "hashed_message" TEXT NOT NULL,
    constraint "fk_nonce" foreign key ("nonce") references "r_values" ("nonce")
);
