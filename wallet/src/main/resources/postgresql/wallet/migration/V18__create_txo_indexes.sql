-- Create indexes to speed up queries filtering by txid and txo_state
CREATE INDEX IF NOT EXISTS idx_txo_spending_info_txid ON txo_spending_info (txid);
CREATE INDEX IF NOT EXISTS idx_txo_spending_info_state ON txo_spending_info (txo_state);

