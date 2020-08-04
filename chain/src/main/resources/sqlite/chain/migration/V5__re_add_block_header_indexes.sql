CREATE INDEX IF NOT EXISTS "block_headers_hash_index" on "block_headers" ("hash");
CREATE INDEX IF NOT EXISTS "block_headers_height_index" on "block_headers" ("height");
