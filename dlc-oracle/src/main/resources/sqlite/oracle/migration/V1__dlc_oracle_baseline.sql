CREATE TABLE `r_values`
(
    `nonce`                VARCHAR(254) NOT NULL,
    `event_name`           VARCHAR(254) NOT NULL UNIQUE,
    `hd_purpose`           INTEGER      NOT NULL,
    `coin`                 INTEGER      NOT NULL,
    `account_index`        INTEGER      NOT NULL,
    `chain_type`           INTEGER      NOT NULL,
    `key_index`            INTEGER      NOT NULL UNIQUE,
    `commitment_signature` VARCHAR(254) NOT NULL,
    PRIMARY KEY (`nonce`)
);

CREATE TABLE `events`
(
    `nonce`           VARCHAR(254) NOT NULL,
    `pubkey`          VARCHAR(254) NOT NULL,
    `event_name`      VARCHAR(254) NOT NULL UNIQUE,
    `num_outcomes`    INTEGER      NOT NULL,
    `signing_version` VARCHAR(254) NOT NULL,
    `maturation_time` TIMESTAMP    NOT NULL,
    `attestation`     VARCHAR(254),
    PRIMARY KEY (`nonce`),
    CONSTRAINT `fk_nonce` FOREIGN KEY (`nonce`) REFERENCES `r_values` (`nonce`) on update NO ACTION on delete NO ACTION,
    CONSTRAINT `fk_label` FOREIGN KEY (`event_name`) REFERENCES `r_values` (`event_name`) on update NO ACTION on delete NO ACTION
);

CREATE TABLE `event_outcomes`
(
    `nonce`          VARCHAR(254) NOT NULL,
    `message`        VARCHAR(254) NOT NULL,
    `hashed_message` VARCHAR(254) NOT NULL,
    CONSTRAINT `fk_nonce` FOREIGN KEY (`nonce`) REFERENCES `events` (`nonce`) on update NO ACTION on delete NO ACTION
);
