ALTER TABLE "tx_table"
    ADD COLUMN "block_hash" TEXT;


UPDATE "tx_table"
set "block_hash" = (
    select "txo_spending_info"."block_hash"
    from "txo_spending_info"
    where "txo_spending_info"."txid" = "tx_table"."txIdBE"
);

-- Delete block_hash column, add spending_tx_id column
ALTER TABLE "txo_spending_info" DROP COLUMN "block_hash";
ALTER TABLE "txo_spending_info" ADD COLUMN "spending_tx_id" TEXT;
