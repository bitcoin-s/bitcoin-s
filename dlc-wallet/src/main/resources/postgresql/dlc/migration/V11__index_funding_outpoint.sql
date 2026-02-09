-- Create an index on funding_outpoint to speed up lookups by funding outpoint
CREATE INDEX IF NOT EXISTS idx_global_dlc_data_funding_outpoint ON global_dlc_data (funding_outpoint);

