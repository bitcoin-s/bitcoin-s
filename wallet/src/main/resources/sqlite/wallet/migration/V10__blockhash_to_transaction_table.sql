ALTER TABLE "tx_table"
    ADD COLUMN "block_hash" VARCHAR(254);


UPDATE "tx_table"
set "block_hash" = (
    select "txo_spending_info"."block_hash"
    from "txo_spending_info"
    where "txo_spending_info"."txid" = "tx_table"."txIdBE"
);

-- Delete block_hash column, add spending_txid column
CREATE TABLE IF NOT EXISTS "txo_spending_info_backup" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"tx_outpoint" VARCHAR(254) NOT NULL, "script_pub_key_id" INT NOT NULL,"value" INTEGER NOT NULL,"hd_privkey_path" VARCHAR(254) NOT NULL,"redeem_script" VARCHAR(254),"script_witness" VARCHAR(254),"txid" VARCHAR(254) NOT NULL,"block_hash" VARCHAR(254), "txo_state" VARCHAR(254) NOT NULL);
INSERT INTO "txo_spending_info_backup" SELECT * FROM "txo_spending_info";
DROP TABLE "txo_spending_info";
CREATE TABLE IF NOT EXISTS "txo_spending_info" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"tx_outpoint" VARCHAR(254) NOT NULL, "script_pub_key_id" INT NOT NULL,"value" INTEGER NOT NULL,"hd_privkey_path" VARCHAR(254) NOT NULL,"redeem_script" VARCHAR(254),"script_witness" VARCHAR(254),"txid" VARCHAR(254) NOT NULL, "txo_state" VARCHAR(254) NOT NULL, spending_txid VARCHAR(254), constraint "fk_scriptPubKey" foreign key("script_pub_key_id") references "pub_key_scripts"("id"), constraint "fk_incoming_txId" foreign key("txid") references "wallet_incoming_txs"("txIdBE") on update NO ACTION on delete NO ACTION);
INSERT INTO "txo_spending_info" ("id","tx_outpoint","script_pub_key_id","value","hd_privkey_path","redeem_script","script_witness","txid","txo_state") SELECT t."id",t."tx_outpoint",t."script_pub_key_id",t.value,t."hd_privkey_path",t."redeem_script",t."script_witness",t."txid",t."txo_state" FROM "txo_spending_info_backup" t;
CREATE INDEX "txo_spending_info_spk_idx" ON "txo_spending_info"("script_pub_key_id");
DROP TABLE "txo_spending_info_backup";
