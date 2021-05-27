CREATE TABLE "global_dlc_data"
(
    "dlc_id"                VARCHAR(254) PRIMARY KEY,
    "temp_contract_id"      VARCHAR(254) NOT NULL UNIQUE,
    "contract_id"           VARCHAR(254) UNIQUE,
    "protocol_version"      INTEGER      NOT NULL,
    "state"                 VARCHAR(254) NOT NULL,
    "is_initiator"          INTEGER      NOT NULL,
    "account"               VARCHAR(254) NOT NULL,
    "change_index"          INTEGER      NOT NULL,
    "key_index"             INTEGER      NOT NULL,

    "fee_rate"              VARCHAR(254) NOT NULL,
    "fund_output_serial_id" VARCHAR(254) NOT NULL,

    "funding_outpoint"      VARCHAR(254),
    "funding_tx_id"         VARCHAR(254),
    "closing_tx_id"         VARCHAR(254),
    "aggregate_signature"   VARCHAR(254)
);

CREATE TABLE "contract_data"
(
    "dlc_id"              VARCHAR(254) PRIMARY KEY,
    "oracle_threshold"    INTEGER      NOT NULL,
    "oracle_params"       VARCHAR(254),
    "contract_descriptor" VARCHAR(254) NOT NULL,
    "contract_maturity"   INTEGER      NOT NULL,
    "contract_timeout"    INTEGER      NOT NULL,
    "total_collateral"    INTEGER      NOT NULL,
    constraint "fk_dlc_id" foreign key ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION
);

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

CREATE TABLE "dlc_announcements"
(
    "dlc_id"          VARCHAR(254) NOT NULL,
    "announcement_id" INTEGER      NOT NULL,
    "index"           INTEGER      NOT NULL,
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
    "announcement_id"        INTEGER      NOT NULL,
    "index"                  INTEGER      NOT NULL,
    "announcement_signature" VARCHAR(254) NOT NULL,
    "nonce"                  VARCHAR(254) NOT NULL UNIQUE,
    "signature"              VARCHAR(254),
    "outcome"                VARCHAR(254),
    constraint "pk_oracle_nonces" primary key ("announcement_id", "index"),
    constraint "fk_announcement_id" foreign key ("announcement_id") references "oracle_announcement_data" ("id") on update NO ACTION on delete NO ACTION
);
CREATE
    INDEX "oracle_nonces_index" on "oracle_nonces" ("nonce");

CREATE TABLE "offer_dlc_data"
(
    "dlc_id"           VARCHAR(254) PRIMARY KEY,
    "funding_pub_key"  VARCHAR(254) NOT NULL,
    "payout_address"   VARCHAR(254) NOT NULL,
    "payout_serial_id" VARCHAR(254) NOT NULL,
    "collateral"       INTEGER      NOT NULL,
    "change_address"   VARCHAR(254) NOT NULL,
    "change_serial_id" VARCHAR(254) NOT NULL,
    constraint "fk_dlc_id" foreign key ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION
);

CREATE TABLE "accept_dlc_data"
(
    "dlc_id"             VARCHAR(254) PRIMARY KEY,
    "funding_pub_key"    VARCHAR(254) NOT NULL,
    "payout_address"     VARCHAR(254) NOT NULL,
    "payout_serial_id"   VARCHAR(254) NOT NULL,
    "collateral"         INTEGER      NOT NULL,
    "change_address"     VARCHAR(254) NOT NULL,
    "change_serial_id"   VARCHAR(254) NOT NULL,
    "negotiation_fields" VARCHAR(254) NOT NULL,
    constraint "fk_dlc_id" foreign key ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION
);

CREATE TABLE "funding_inputs"
(
    "out_point"          VARCHAR(254) PRIMARY KEY,
    "dlc_id"             VARCHAR(254) NOT NULL,
    "is_initiator"       INTEGER      NOT NULL,
    "input_serial_id"    VARCHAR(254) NOT NULL,
    "output"             VARCHAR(254) NOT NULL,
    "max_witness_length" INTEGER      NOT NULL,
    "redeem_script_opt"  VARCHAR(254),
    "witness_script_opt" VARCHAR(254),
    constraint "fk_dlc_id" foreign key ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION
);

CREATE TABLE "cet_sigs"
(
    "dlc_id"        VARCHAR(254) NOT NULL,
    "index"         INTEGER      NOT NULL,
    "sig_point"     VARCHAR(254) NOT NULL,
    "accepter_sig"  VARCHAR(254) NOT NULL,
    "initiator_sig" VARCHAR(254),
    constraint "pk_cet_sigs" primary key ("dlc_id", "index"),
    constraint "fk_dlc_id" foreign key ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION
);

CREATE TABLE "refund_sigs"
(
    "dlc_id"        VARCHAR(254) PRIMARY KEY,
    "accepter_sig"  VARCHAR(254) NOT NULL,
    "initiator_sig" VARCHAR(254),
    constraint "fk_dlc_id" foreign key ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION
);

CREATE TABLE "watch_only_tx_table"
(
    "txIdBE"         VARCHAR(254) NOT NULL PRIMARY KEY,
    "transaction"    VARCHAR(254) NOT NULL,
    "unsignedTxIdBE" VARCHAR(254) NOT NULL,
    "unsignedTx"     VARCHAR(254) NOT NULL,
    "wTxIdBE"        VARCHAR(254),
    "totalOutput"    INTEGER      NOT NULL,
    "numInputs"      INTEGER      NOT NULL,
    "numOutputs"     INTEGER      NOT NULL,
    "locktime"       INTEGER      NOT NULL,
    "block_hash"     VARCHAR(254)
);
