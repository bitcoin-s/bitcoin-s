CREATE TABLE "pub_key_scripts" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, "script_pub_key" TEXT NOT NULL UNIQUE, "script_type" TEXT NOT NULL);
INSERT INTO "pub_key_scripts" ("script_pub_key", "script_type") SELECT DISTINCT "script_pub_key", "script_type" FROM "addresses";

CREATE TABLE IF NOT EXISTS "txo_spending_info_backup" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"tx_outpoint" VARCHAR(254) NOT NULL, "script_pub_key" VARCHAR(254) NOT NULL,"value" INTEGER NOT NULL,"hd_privkey_path" VARCHAR(254) NOT NULL,"redeem_script" VARCHAR(254),"script_witness" VARCHAR(254),"txid" VARCHAR(254) NOT NULL,"block_hash" VARCHAR(254), "txo_state" VARCHAR(254) NOT NULL);
INSERT INTO "txo_spending_info_backup" SELECT * FROM "txo_spending_info";
DROP TABLE "txo_spending_info";
CREATE TABLE IF NOT EXISTS "txo_spending_info" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"tx_outpoint" TEXT NOT NULL, "script_pub_key_id" INT NOT NULL,"value" INTEGER NOT NULL,"hd_privkey_path" TEXT NOT NULL,"redeem_script" TEXT,"script_witness" TEXT,"txid" VARCHAR(254) NOT NULL,"block_hash" VARCHAR(254), "txo_state" TEXT NOT NULL, constraint "fk_scriptPubKey" foreign key("script_pub_key_id") references "pub_key_scripts"("id"), constraint "fk_incoming_txId" foreign key("txid") references "wallet_incoming_txs"("txIdBE") on update NO ACTION on delete NO ACTION);
INSERT INTO "txo_spending_info" ("id","tx_outpoint","script_pub_key_id","hd_privkey_path","redeem_script","script_witness","txid","block_hash","txo_state") SELECT t."id",t."tx_outpoint",s."id",t."hd_privkey_path",t."redeem_script",t."script_witness",t."txid",t."block_hash",t."txo_state" FROM "txo_spending_info_backup" t, "pub_key_scripts" s WHERE s."script_pub_key" = t."script_pub_key";
CREATE INDEX "txo_spending_info_spk_idx" ON "txo_spending_info"("script_pub_key_id");
DROP TABLE "txo_spending_info_backup";

CREATE TABLE IF NOT EXISTS addresses_backup ("hd_purpose" INTEGER NOT NULL,"account_index" INTEGER NOT NULL,"hd_coin" INTEGER NOT NULL,"hd_chain_type" INTEGER NOT NULL,"address" VARCHAR(254) PRIMARY KEY NOT NULL,"script_witness" VARCHAR(254),"script_pub_key" VARCHAR(254) NOT NULL UNIQUE,"address_index" INTEGER NOT NULL,"pubkey" VARCHAR(254) NOT NULL,"hashed_pubkey" VARCHAR(254) NOT NULL,"script_type" VARCHAR(254) NOT NULL,constraint "fk_account" foreign key("hd_purpose","hd_coin","account_index") references "wallet_accounts"("hd_purpose","coin","account_index") on update NO ACTION on delete NO ACTION);
INSERT INTO addresses_backup SELECT * FROM "addresses";
DROP TABLE "addresses";
CREATE TABLE IF NOT EXISTS "addresses" ("hd_purpose" INTEGER NOT NULL,"account_index" INTEGER NOT NULL,"hd_coin" INTEGER NOT NULL,"hd_chain_type" INTEGER NOT NULL,"address" TEXT PRIMARY KEY NOT NULL,"script_witness" TEXT,"script_pub_key_id" INT NOT NULL,"address_index" INTEGER NOT NULL,"pubkey" TEXT NOT NULL,"hashed_pubkey" TEXT NOT NULL,constraint "fk_spk" foreign key("script_pub_key_id") references "pub_key_scripts"("id") on update NO ACTION on delete NO ACTION);
INSERT INTO "addresses" ("hd_purpose","account_index","hd_coin","hd_chain_type","address","script_witness","script_pub_key_id","address_index","pubkey","hashed_pubkey") SELECT a."hd_purpose",a."account_index",a."hd_coin",a."hd_chain_type",a."address",a."script_witness",s."id",a."address_index",a."pubkey",a."hashed_pubkey" FROM "addresses_backup" a, "pub_key_scripts" s WHERE s."script_pub_key" = a."script_pub_key";
CREATE INDEX "address_spk_idx" ON "addresses"("script_pub_key_id");
CREATE INDEX "address_account_idx" ON "addresses"("hd_purpose","account_index","hd_coin");
DROP TABLE "addresses_backup";

-- wallet_address_tags_backup.fk_address was dropped along with address table, and we need to recreate it
-- SQLite dosn't allow to create foreign key for existing tables, so we recreate the whole table here
CREATE TABLE "wallet_address_tags_backup" ("address" VARCHAR(254) NOT NULL,"tag_name" VARCHAR(254) NOT NULL,"tag_type" VARCHAR(254) NOT NULL,constraint "pk_address_tags" primary key ("address", "tag_type"), constraint "fk_address" foreign key("address") references "addresses"("address") on update NO ACTION on delete NO ACTION);
INSERT INTO "wallet_address_tags_backup" SELECT * FROM "wallet_address_tags";
DROP TABLE "wallet_address_tags";
CREATE TABLE "wallet_address_tags" ("address" VARCHAR(254) NOT NULL,"tag_name" VARCHAR(254) NOT NULL,"tag_type" VARCHAR(254) NOT NULL,constraint "pk_address_tags" primary key ("address", "tag_type"), constraint "fk_address" foreign key("address") references "addresses"("address") on update NO ACTION on delete NO ACTION);
INSERT INTO "wallet_address_tags" SELECT * FROM "wallet_address_tags_backup";
DROP TABLE "wallet_address_tags_backup";