ALTER TABLE pub_key_scripts ADD COLUMN hash VARCHAR(64);

ALTER TABLE pub_key_scripts DROP CONSTRAINT pub_key_scripts_script_pub_key_key;

CREATE UNIQUE INDEX pub_key_scripts_hash_idx ON pub_key_scripts(hash);
