ALTER TABLE "txo_spending_info" ADD COLUMN "txo_state" VARCHAR(254);

UPDATE "txo_spending_info" SET "txo_state" = "PendingConfirmationsSpent" WHERE "spent" = 1;
UPDATE "txo_spending_info" SET "txo_state" = "PendingConfirmationsReceived" WHERE "spent" = 0;
UPDATE "txo_spending_info" SET "confirmations" = 0 WHERE "confirmations" = NULL;

-- This block drops the "spent" column
CREATE TEMPORARY TABLE "txo_spending_info_backup" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"tx_outpoint" VARCHAR(254) NOT NULL, "script_pub_key" VARCHAR(254) NOT NULL,"value" INTEGER NOT NULL,"hd_privkey_path" VARCHAR(254) NOT NULL,"redeem_script" VARCHAR(254),"script_witness" VARCHAR(254),"confirmations" INTEGER,"txid" VARCHAR(254) NOT NULL,"block_hash" VARCHAR(254), "txo_state" VARCHAR(254) NOT NULL, constraint "fk_scriptPubKey" foreign key("script_pub_key") references "addresses"("script_pub_key"));
INSERT INTO "txo_spending_info_backup" SELECT "id", "tx_outpoint", "script_pub_key", "value", "hd_privkey_path", "redeem_script", "script_witness", "confirmations", "txid","block_hash", "txo_state" FROM "txo_spending_info";
DROP TABLE "txo_spending_info";
CREATE TABLE "txo_spending_info" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"tx_outpoint" VARCHAR(254) NOT NULL, "script_pub_key" VARCHAR(254) NOT NULL,"value" INTEGER NOT NULL,"hd_privkey_path" VARCHAR(254) NOT NULL,"redeem_script" VARCHAR(254),"script_witness" VARCHAR(254),"confirmations" INTEGER,"txid" VARCHAR(254) NOT NULL,"block_hash" VARCHAR(254), "txo_state" VARCHAR(254) NOT NULL, constraint "fk_scriptPubKey" foreign key("script_pub_key") references "addresses"("script_pub_key") on update NO ACTION on delete NO ACTION);
INSERT INTO "txo_spending_info" SELECT "id", "tx_outpoint", "script_pub_key", "value", "hd_privkey_path", "redeem_script", "script_witness", "confirmations", "txid","block_hash", "txo_state" FROM "txo_spending_info_backup";
DROP TABLE "txo_spending_info_backup";
