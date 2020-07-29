-- This block changes the chain work column from a VARBINARY to a VARCHAR
CREATE TABLE "block_headers_temp" ("height" INTEGER NOT NULL,"hash" VARCHAR(254) PRIMARY KEY NOT NULL,"version" INTEGER NOT NULL,"previous_block_hash" VARCHAR(254) NOT NULL,"merkle_root_hash" VARCHAR(254) NOT NULL,"time" INTEGER NOT NULL,"n_bits" INTEGER NOT NULL,"nonce" INTEGER NOT NULL,"hex" VARCHAR(254) NOT NULL, "chain_work" VARCHAR(33) NOT NULL DEFAULT "0000000000000000000000000000000000000000000000000000000000000000");
INSERT INTO "block_headers_temp" SELECT "height", "hash", "version", "previous_block_hash", "merkle_root_hash", "time", "n_bits", "nonce", "hex", hex("chain_work") FROM "block_headers";
DROP TABLE "block_headers";
CREATE TABLE "block_headers" ("height" INTEGER NOT NULL,"hash" VARCHAR(254) PRIMARY KEY NOT NULL,"version" INTEGER NOT NULL,"previous_block_hash" VARCHAR(254) NOT NULL,"merkle_root_hash" VARCHAR(254) NOT NULL,"time" INTEGER NOT NULL,"n_bits" INTEGER NOT NULL,"nonce" INTEGER NOT NULL,"hex" VARCHAR(254) NOT NULL, "chain_work" VARCHAR(33) NOT NULL DEFAULT "0000000000000000000000000000000000000000000000000000000000000000");
INSERT INTO "block_headers" SELECT "height", "hash", "version", "previous_block_hash", "merkle_root_hash", "time", "n_bits", "nonce", "hex","chain_work"FROM "block_headers_temp";
DROP TABLE "block_headers_temp";

CREATE INDEX "block_headers_chain_work_index" on "block_headers" ("chain_work");
