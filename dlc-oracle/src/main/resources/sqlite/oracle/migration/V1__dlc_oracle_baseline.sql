CREATE TABLE "r_values"
(
    "nonce"         VARCHAR(254) PRIMARY KEY,
    "hd_purpose"    INTEGER NOT NULL,
    "coin"          INTEGER NOT NULL,
    "account_index" INTEGER NOT NULL,
    "chain_type"    INTEGER NOT NULL,
    "key_index"     INTEGER NOT NULL
);

CREATE TABLE "events"
(
    "nonce"           VARCHAR(254) PRIMARY KEY,
    "label"           VARCHAR(254) UNIQUE NOT NULL,
    "num_outcomes"    INTEGER             NOT NULL,
    "signing_version" INTEGER             NOT NULL,
    "attestation"     VARCHAR(254),
    constraint "fk_nonce" foreign key ("nonce") references "r_values" ("nonce")
);

CREATE TABLE "event_outcomes"
(
    "nonce"          VARCHAR(254) PRIMARY KEY,
    "message"        VARCHAR(254) NOT NULL,
    "hashed_message" VARCHAR(254) NOT NULL,
    constraint "fk_nonce" foreign key ("nonce") references "r_values" ("nonce")
);
