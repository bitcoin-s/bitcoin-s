ALTER TABLE broadcast_elements DROP COLUMN id;
ALTER TABLE broadcast_elements ADD CONSTRAINT pk_broadcast_tx_id PRIMARY KEY (txid);
