ALTER TABLE funding_inputs
    ADD COLUMN sequence BIGINT NOT NULL DEFAULT 4294967295; -- default is UInt32.max, the old assumed value
