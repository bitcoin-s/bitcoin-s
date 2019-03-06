CREATE TABLE IF NOT EXISTS "block_headers" ("height" INTEGER NOT NULL,"hash" VARCHAR PRIMARY KEY NOT NULL,"version" BIGINT NOT NULL,"previous_block_hash" VARCHAR NOT NULL,"merkle_root_hash" VARCHAR NOT NULL,"time" BIGINT NOT NULL,"n_bits" BIGINT NOT NULL,"nonce" BIGINT NOT NULL,"hex" VARCHAR(254) NOT NULL);
CREATE INDEX "height_index" on "block_headers" ("height");
