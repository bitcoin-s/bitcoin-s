--changes pk to (address,tag_name) rather than (address,tag_type)
ALTER TABLE wallet_address_tags DROP CONSTRAINT IF EXISTS "pk_address_tags";

ALTER TABLE wallet_address_tags ADD CONSTRAINT pk_address_tags PRIMARY KEY (address, tag_name);