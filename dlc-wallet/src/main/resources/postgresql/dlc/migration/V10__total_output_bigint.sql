ALTER TABLE "watch_only_tx_table"
ALTER COLUMN "totalOutput" TYPE BIGINT;

ALTER TABLE "contract_data"
ALTER COLUMN "total_collateral" TYPE BIGINT;

ALTER TABLE "contract_data"
ALTER COLUMN "contract_maturity" TYPE BIGINT;

ALTER TABLE "contract_data"
ALTER COLUMN "contract_timeout" TYPE BIGINT;