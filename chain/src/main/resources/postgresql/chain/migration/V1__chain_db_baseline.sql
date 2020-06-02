CREATE TABLE IF NOT EXISTS block_headers (height BIGINT NOT NULL,hash VARCHAR(254) PRIMARY KEY NOT NULL,version INTEGER NOT NULL,previous_block_hash VARCHAR(254) NOT NULL,merkle_root_hash VARCHAR(254) NOT NULL,time BIGINT NOT NULL,n_bits BIGINT NOT NULL,nonce BIGINT NOT NULL,hex VARCHAR(254) NOT NULL);
CREATE INDEX IF NOT EXISTS block_headers_hash_index on block_headers (hash);
CREATE INDEX IF NOT EXISTS block_headers_height_index on block_headers (height);

CREATE TABLE IF NOT EXISTS cfheaders (hash VARCHAR(254) PRIMARY KEY NOT NULL,filter_hash VARCHAR(254) NOT NULL,previous_filter_header VARCHAR(254) NOT NULL,block_hash VARCHAR(254) NOT NULL,height BIGINT NOT NULL);
CREATE INDEX IF NOT EXISTS cfheaders_block_hash_index on cfheaders (block_hash);
CREATE INDEX IF NOT EXISTS cfheaders_height_index on cfheaders (height);
ALTER TABLE cfheaders ADD CONSTRAINT cfh_block_hash_fk FOREIGN KEY (block_hash) REFERENCES block_headers(hash);

CREATE TABLE IF NOT EXISTS cfilters (hash VARCHAR(254) NOT NULL,filter_type INTEGER NOT NULL,bytes VARCHAR(254) NOT NULL,height BIGINT NOT NULL,block_hash VARCHAR(254) PRIMARY KEY NOT NULL);
CREATE INDEX IF NOT EXISTS cfilters_hash_index on cfilters (hash);
CREATE INDEX IF NOT EXISTS cfilters_height_index on cfilters (height);
ALTER TABLE cfilters ADD CONSTRAINT cf_block_hash_fk FOREIGN KEY (block_hash) REFERENCES block_headers(hash);
