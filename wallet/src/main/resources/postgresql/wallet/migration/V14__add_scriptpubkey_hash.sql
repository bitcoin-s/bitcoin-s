ALTER TABLE "pub_key_scripts" ADD COLUMN hash VARCHAR(64);

CREATE UNIQUE INDEX "pub_key_scripts_hash_idx" ON "pub_key_scripts"(hash);
