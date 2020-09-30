CREATE TABLE r_values
(
    nonce                TEXT    NOT NULL,
    label                TEXT    NOT NULL UNIQUE,
    hd_purpose           INTEGER NOT NULL,
    coin                 INTEGER NOT NULL,
    account_index        INTEGER NOT NULL,
    chain_type           INTEGER NOT NULL,
    key_index            INTEGER NOT NULL,
    commitment_signature TEXT    NOT NULL,
    PRIMARY KEY (nonce)
);

CREATE TABLE events
(
    nonce           TEXT    NOT NULL,
    label           TEXT    NOT NULL UNIQUE,
    num_outcomes    INTEGER NOT NULL,
    signing_version TEXT    NOT NULL,
    attestation     TEXT,
    CONSTRAINT fk_label FOREIGN KEY (label) REFERENCES r_values (label) on update NO ACTION on delete NO ACTION,
    PRIMARY KEY (nonce),
    CONSTRAINT fk_nonce FOREIGN KEY (nonce) REFERENCES r_values (nonce) on update NO ACTION on delete NO ACTION
);

CREATE TABLE event_outcomes
(
    nonce          TEXT NOT NULL,
    message        TEXT NOT NULL,
    hashed_message TEXT NOT NULL,
    CONSTRAINT fk_nonce FOREIGN KEY (nonce) REFERENCES events (nonce) on update NO ACTION on delete NO ACTION
);
