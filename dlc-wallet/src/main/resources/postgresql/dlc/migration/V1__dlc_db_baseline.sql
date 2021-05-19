CREATE TABLE "global_dlc_data"
(
    "dlc_id"                TEXT PRIMARY KEY,
    "temp_contract_id"      TEXT    NOT NULL UNIQUE,
    "contract_id"           TEXT UNIQUE,
    "protocol_version"      INTEGER NOT NULL,
    "state"                 TEXT    NOT NULL,
    "is_initiator"          INTEGER NOT NULL,
    "account"               TEXT    NOT NULL,
    "change_index"          INTEGER NOT NULL,
    "key_index"             INTEGER NOT NULL,

    "oracle_threshold"      INTEGER NOT NULL,
    "oracle_params"         TEXT,
    "contract_descriptor"   TEXT    NOT NULL,
    "contract_maturity"     INTEGER NOT NULL,
    "contract_timeout"      INTEGER NOT NULL,
    "total_collateral"      INTEGER NOT NULL,
    "fee_rate"              TEXT    NOT NULL,
    "fund_output_serial_id" INTEGER NOT NULL,

    "funding_outpoint"      TEXT,
    "funding_tx_id"         TEXT,
    "closing_tx_id"         TEXT,
    "aggregate_signature"   TEXT
);

CREATE TABLE "oracle_announcement_data"
(
    "id"                     SERIAL PRIMARY KEY NOT NULL,
    "announcement_signature" TEXT               NOT NULL UNIQUE,
    "pub_key"                TEXT               NOT NULL,
    "signing_pub_key"        TEXT               NOT NULL,
    "event_maturity"         INTEGER            NOT NULL,
    "event_id"               TEXT               NOT NULL,
    "event_descriptor"       TEXT               NOT NULL
);
CREATE
    INDEX "oracle_announcements_pub_key_index" on "oracle_announcement_data" ("pub_key");

CREATE TABLE "dlc_announcements"
(
    "dlc_id"          TEXT    NOT NULL,
    "announcement_id" INTEGER NOT NULL,
    "index"           INTEGER NOT NULL,
    "used"            INTEGER, -- if signatures used for execution
    constraint "pk_announcement_id_index" primary key ("dlc_id", "announcement_id"),
    constraint "fk_dlc_id" foreign key ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION,
    constraint "fk_announcement_id" foreign key ("announcement_id") references "oracle_announcement_data" ("id") on update NO ACTION on delete NO ACTION
);
CREATE
    INDEX "dlc_announcements_dlc_id_index" on "dlc_announcements" ("dlc_id");
CREATE
    INDEX "dlc_announcements_announcement_id_index" on "dlc_announcements" ("announcement_id");

CREATE TABLE "oracle_nonces"
(
    "announcement_id"        INTEGER NOT NULL,
    "index"                  INTEGER NOT NULL,
    "announcement_signature" TEXT    NOT NULL,
    "nonce"                  TEXT    NOT NULL UNIQUE,
    "signature"              TEXT,
    "outcome"                TEXT,
    constraint "pk_oracle_nonces" primary key ("announcement_id", "index"),
    constraint "fk_announcement_id" foreign key ("announcement_id") references "oracle_announcement_data" ("id") on update NO ACTION on delete NO ACTION
);
CREATE
    INDEX "oracle_nonces_index" on "oracle_nonces" ("nonce");

CREATE TABLE "offer_dlc_data"
(
    "dlc_id"           TEXT PRIMARY KEY,
    "funding_pub_key"  TEXT    NOT NULL,
    "payout_address"   TEXT    NOT NULL,
    "payout_serial_id" INTEGER NOT NULL,
    "collateral"       INTEGER NOT NULL,
    "change_address"   TEXT    NOT NULL,
    "change_serial_id" INTEGER NOT NULL,
    constraint "fk_dlc_id" foreign key ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION
);

CREATE TABLE "accept_dlc_data"
(
    "dlc_id"           TEXT PRIMARY KEY,
    "funding_pub_key"  TEXT    NOT NULL,
    "payout_address"   TEXT    NOT NULL,
    "payout_serial_id" INTEGER NOT NULL,
    "collateral"       INTEGER NOT NULL,
    "change_address"   TEXT    NOT NULL,
    "change_serial_id" INTEGER NOT NULL,
    constraint "fk_dlc_id" foreign key ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION
);

CREATE TABLE "funding_inputs"
(
    "out_point"          TEXT PRIMARY KEY,
    "dlc_id"             TEXT    NOT NULL,
    "is_initiator"       INTEGER NOT NULL,
    "input_serial_id"    INTEGER NOT NULL,
    "output"             TEXT    NOT NULL,
    "max_witness_length" INTEGER NOT NULL,
    "redeem_script_opt"  TEXT,
    "witness_script_opt" TEXT,
    constraint "fk_dlc_id" foreign key ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION
);

CREATE TABLE "cet_sigs"
(
    "dlc_id"        TEXT    NOT NULL,
    "index"         INTEGER NOT NULL,
    "sig_point"     TEXT    NOT NULL,
    "accept_sig"    TEXT    NOT NULL,
    "initiator_sig" TEXT,
    constraint "pk_cet_sigs" primary key ("dlc_id", "index"),
    constraint "fk_dlc_id" foreign key ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION
);

CREATE TABLE "refund_sigs"
(
    "dlc_id"        TEXT PRIMARY KEY,
    "accept_sig"    TEXT NOT NULL,
    "initiator_sig" TEXT,
    constraint "fk_dlc_id" foreign key ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION
);

CREATE TABLE "watch_only_tx_table"
(
    "txIdBE"         TEXT    NOT NULL PRIMARY KEY,
    "transaction"    TEXT    NOT NULL,
    "unsignedTxIdBE" TEXT    NOT NULL,
    "unsignedTx"     TEXT    NOT NULL,
    "wTxIdBE"        TEXT,
    "totalOutput"    INTEGER NOT NULL,
    "numInputs"      INTEGER NOT NULL,
    "numOutputs"     INTEGER NOT NULL,
    "locktime"       INTEGER NOT NULL,
    "block_hash"     TEXT
);
