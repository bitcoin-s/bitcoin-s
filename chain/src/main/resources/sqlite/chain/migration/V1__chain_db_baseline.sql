CREATE TABLE IF NOT EXISTS "block_headers" ("height" INTEGER NOT NULL,"hash" VARCHAR(254) PRIMARY KEY NOT NULL,"version" INTEGER NOT NULL,"previous_block_hash" VARCHAR(254) NOT NULL,"merkle_root_hash" VARCHAR(254) NOT NULL,"time" INTEGER NOT NULL,"n_bits" INTEGER NOT NULL,"nonce" INTEGER NOT NULL,"hex" VARCHAR(254) NOT NULL);
CREATE INDEX "block_headers_hash_index" on "block_headers" ("hash");
CREATE INDEX "block_headers_height_index" on "block_headers" ("height");

CREATE TABLE IF NOT EXISTS "cfheaders" ("hash" VARCHAR(254) PRIMARY KEY NOT NULL,"filter_hash" VARCHAR(254) NOT NULL,"previous_filter_header" VARCHAR(254) NOT NULL,"block_hash" VARCHAR(254) NOT NULL,"height" INTEGER NOT NULL);
CREATE INDEX "cfheaders_block_hash_index" on "cfheaders" ("block_hash");
CREATE INDEX "cfheaders_height_index" on "cfheaders" ("height");

CREATE TABLE IF NOT EXISTS "cfilters" ("hash" VARCHAR(254) NOT NULL,"filter_type" INTEGER NOT NULL,"bytes" VARCHAR(254) NOT NULL,"height" INTEGER NOT NULL,"block_hash" VARCHAR(254) PRIMARY KEY NOT NULL);
CREATE INDEX "cfilters_hash_index" on "cfilters" ("hash");
CREATE INDEX "cfilters_height_index" on "cfilters" ("height");
